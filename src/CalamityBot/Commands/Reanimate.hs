{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Reanimate Fuckery
module CalamityBot.Commands.Reanimate (
  reanimateGroup,
) where

import Calamity
import Calamity.Commands as C
import Calamity.Commands.Context (FullContext)
import CalamityBot.Utils.Reanimate
import Codec.Picture
import Control.Lens
import qualified Data.ByteString as B
import Data.Complex
import qualified Data.Text as T
import Graphics.SvgTree
import qualified Graphics.SvgTree.Types
import Linear.V2
import Network.Mime (defaultMimeLookup)
import qualified Network.HTTP.Req as Req
import qualified Text.URI as URI
import qualified Polysemy as P
import Reanimate
import Reanimate.Render (Format (RenderGif, RenderWebm, RenderMp4), Raster (RasterAuto))

protectText :: T.Text -> T.Text
protectText = T.concatMap (fromString . protectChar)

protectChar :: Char -> String
protectChar '#' = "\\#"
protectChar '$' = "\\$"
protectChar '%' = "\\%"
protectChar '^' = "\\^{}"
protectChar '&' = "\\&"
protectChar '{' = "\\{"
protectChar '}' = "\\}"
protectChar '~' = "\\~{}"
protectChar '\\' = "\\textbackslash{}"
protectChar '_' = "\\_{}"
protectChar x = [x]

renderText :: T.Text -> Animation
renderText text =
  let rt = center . latex $ text
      rtRotated = mkAnimation 5 (\t -> rotateAroundCenter (t * 360) rt)
   in addStatic (mkBackground "white") rtRotated

findSVG :: [Attachment] -> Maybe Attachment
findSVG = find (\a -> "image/svg" `B.isPrefixOf` defaultMimeLookup (a ^. #filename))

usableTree :: Tree -> Bool
usableTree None = False
usableTree (UseTree _ _) = False
usableTree (DefinitionTree _) = False
usableTree _ = True

reanimateGroup :: BotC r => P.Sem (DSLState FullContext r) ()
reanimateGroup = void
  . help (const "Commands related to reanimate fuckery")
  . C.group "reanimate"
  $ do
    help (const "Render a message") $
      command @'[KleenePlusConcat T.Text] "render" \ctx msg -> do
        let anim = renderText msg
        r <- P.embed $ renderToMemory anim RasterAuto RenderGif 400 400 30
        case r of
          Right s -> do
            void $ tell ctx (CreateMessageAttachment "lol.gif" Nothing s)
          Left e ->
            putLBSLn $ "Failed with reason: " <> e

    help (const "Render a fourier thing of an svg") $
      command @'[] "renders" \ctx -> do
        case findSVG (ctx ^. #message . #attachments) of
          Just svg -> do
            Just uri <- pure $ URI.mkURI (svg ^. #url)
            Just (url, options) <- pure $ Req.useHttpsURI uri
            r <- P.embed . Req.runReq Req.defaultHttpConfig $ Req.req Req.GET url Req.NoReqBody Req.lbsResponse options
            let file = Req.responseBody r
            Just doc <- pure $ parseSvgFile "a.svg" (decodeUtf8 file)
            let tree = head . fromList . filter usableTree $ doc ^. Graphics.SvgTree.Types.documentElements
            let f = mkSVGLatex $ flipYAxis tree
            let anim = setDuration 20 $
                  scene $ do
                    _ <- newSpriteSVG $ mkBackgroundPixel (PixelRGBA8 252 252 252 0xFF)
                    play $
                      fourierA f (fromToS 0 5) -- Rotate 15 times
                        & setDuration 30
                        & signalA (reverseS . powerS 2 . reverseS) -- Start fast, end slow
                        & pauseAtEnd 2
                    play $
                      fourierA f (constantS 0) -- Don't rotate at all
                        & setDuration 10
                        & reverseA
                        & signalA (powerS 2) -- Start slow, end fast
                        & pauseAtEnd 2
            r <- P.embed $ renderToMemory anim RasterAuto RenderGif 480 360 15
            case r of
              Right s -> do
                void $ tell ctx (CreateMessageAttachment "lol.gif" Nothing s)
              Left e ->
                putLBSLn $ "Failed with reason: " <> e
          Nothing ->
            void $ tell @T.Text ctx "Couldn't find an svg"

    help (const "Render a fourier thing") $
      command @'[KleenePlusConcat T.Text] "renderf" \ctx msg -> do
        let f = mkFourierLatex msg
        let anim = setDuration 20 $
              scene $ do
                _ <- newSpriteSVG $ mkBackgroundPixel (PixelRGBA8 252 252 252 0xFF)
                play $
                  fourierA f (fromToS 0 5) -- Rotate 15 times
                    & setDuration 30
                    & signalA (reverseS . powerS 2 . reverseS) -- Start fast, end slow
                    & pauseAtEnd 2
                play $
                  fourierA f (constantS 0) -- Don't rotate at all
                    & setDuration 10
                    & reverseA
                    & signalA (powerS 2) -- Start slow, end fast
                    & pauseAtEnd 2
        r <- P.embed $ renderToMemory anim RasterAuto RenderGif 480 360 15
        case r of
          Right s -> do
            void $ tell ctx (CreateMessageAttachment "lol.gif" Nothing s)
          Left e ->
            putLBSLn $ "Failed with reason: " <> e

scaleToFit :: Double -> Double -> Tree -> Tree
scaleToFit w h t =
  let oh = svgHeight t
      ow = svgWidth t
   in scale (min (w / ow) (h / oh)) t

-- layer 2
fourierA :: Fourier -> (Double -> Double) -> Animation
fourierA f genPhi = animate $ \t ->
  let circles = setFourierLength (t * fourierLen f) f
      coeffs = fourierCoefficients $ rotateFourier (genPhi t) circles
   in mkGroup
        [ drawCircles coeffs
        , withStrokeColor "green" $
            withStrokeLineJoin JoinRound $
              withFillOpacity 0 $
                withStrokeWidth (defaultStrokeWidth * 2) $
                  mkLinePath $ mkFourierOutline circles
        , let x :+ y = sum coeffs
           in translate x y $ withFillColor "red" $ mkCircle (defaultStrokeWidth * 3)
        ]

drawCircles :: [Complex Double] -> SVG
drawCircles [] = mkGroup []
drawCircles (x :+ y : xs) =
  translate x y $ drawCircles' xs

drawCircles' :: [Complex Double] -> SVG
drawCircles' circles =
  mkGroup
    [ worker circles
    , withStrokeColor "black" $
        withStrokeLineJoin JoinRound $
          withFillOpacity 0 $
            mkLinePath [(x, y) | x :+ y <- scanl (+) 0 circles]
    ]
  where
    worker [] = None
    worker (x :+ y : rest) =
      let radius = sqrt (x * x + y * y)
       in mkGroup
            [ withStrokeColor "dimgrey" $
                withFillOpacity 0 $
                  mkCircle radius
            , translate x y $ worker rest
            ]

-- layer 1
newtype Fourier = Fourier {fourierCoefficients :: [Complex Double]}

mkSVGLatex :: Tree -> Fourier
mkSVGLatex t =
  mkFourier $
    lineToPoints 500 $
      toLineCommands $ extractPath $ center $ scaleToFit 9.6 7.2 t

mkFourierLatex :: T.Text -> Fourier
mkFourierLatex t =
  mkFourier $
    lineToPoints 500 $
      toLineCommands $ extractPath $ center $ scaleToFit 9.6 7.2 $ latex t

fourierLen :: Fourier -> Double
fourierLen f = sum $ map magnitude $ drop 1 $ take 500 $ fourierCoefficients f

pointAtFourier :: Fourier -> Complex Double
pointAtFourier = sum . fourierCoefficients

mkFourier :: [RPoint] -> Fourier
mkFourier points =
  Fourier $
    findCoefficient 0 :
    concat [[findCoefficient n, findCoefficient (- n)] | n <- [1 ..]]
  where
    findCoefficient :: Int -> Complex Double
    findCoefficient n =
      sum
        [ toComplex point * exp (negate (fromIntegral n) * 2 * pi * i * t) * deltaT
        | (idx, point) <- zip [0 :: Int ..] points
        , let t = fromIntegral idx / nPoints
        ]
    i = 0 :+ 1
    toComplex (V2 x y) = x :+ y
    deltaT = recip nPoints
    nPoints = fromIntegral (length points)

setFourierLength :: Double -> Fourier -> Fourier
setFourierLength _ (Fourier []) = Fourier []
setFourierLength len0 (Fourier (first : lst)) = Fourier $ first : worker len0 lst
  where
    worker _len [] = []
    worker len (c : cs) =
      if magnitude c < len
        then c : worker (len - magnitude c) cs
        else [c * realToFrac (len / magnitude c)]

rotateFourier :: Double -> Fourier -> Fourier
rotateFourier phi (Fourier coeffs) =
  Fourier $ worker coeffs (0 :: Integer)
  where
    worker [] _ = []
    worker (x : rest) 0 = x : worker rest 1
    worker [left] n = worker [left, 0] n
    worker (left : right : rest) n =
      let n' = fromIntegral n
       in left * exp (negate n' * 2 * pi * i * phi') :
          right * exp (n' * 2 * pi * i * phi') :
          worker rest (n + 1)
    i = 0 :+ 1
    phi' = realToFrac phi

mkFourierOutline :: Fourier -> [(Double, Double)]
mkFourierOutline fourier =
  [ (x, y)
  | idx <- [0 .. granularity]
  , let x :+ y = pointAtFourier $ rotateFourier (idx / granularity) fourier
  ]
  where
    granularity = 500
