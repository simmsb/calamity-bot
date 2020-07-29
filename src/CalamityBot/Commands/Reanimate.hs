-- | Reanimate Fuckery
module CalamityBot.Commands.Reanimate
    ( reanimateGroup,
    ) where

import Calamity.Commands
import Calamity
import CalamityBot.Commands.Reanimate.RenderInMem
import qualified Data.Text.Lazy as L
import qualified Polysemy as P
import Reanimate
import Reanimate.LaTeX (latex)
import Reanimate.Render (Raster(RasterAuto), Format(RenderGif))
import Reanimate.Animation (animate)
import Data.Default.Class (def)

protectText :: L.Text -> L.Text
protectText = L.concatMap (fromString . protectChar)

protectChar :: Char -> String
protectChar '#'  = "\\#"
protectChar '$'  = "\\$"
protectChar '%'  = "\\%"
protectChar '^'  = "\\^{}"
protectChar '&'  = "\\&"
protectChar '{'  = "\\{"
protectChar '}'  = "\\}"
protectChar '~'  = "\\~{}"
protectChar '\\' = "\\textbackslash{}"
protectChar '_'  = "\\_{}"
protectChar x = [x]


renderText :: L.Text -> Animation
renderText text =
  let rt = latex . L.toStrict . protectText $ text
      rtRotated = mkAnimation 5 (\t -> rotateAroundCenter (t * 360) rt)
  in addStatic (mkBackground "white") rtRotated

reanimateGroup :: BotC r => P.Sem (DSLState r) ()
reanimateGroup = void
  . help (const "Commands related to reanimate fuckery")
  . group "reanimate"
  $ do
    help (const "Render a message") $
      command @'[KleenePlusConcat L.Text] "render" \ctx msg -> do
        let anim = renderText msg
        s <- P.embed $ renderToMemory anim RasterAuto RenderGif 400 400 30
        case s of
          Right s' -> do
            void $ tell ctx (TFile "lol.gif" $ fromStrict s')
          Left r ->
            print $ "Failed with reason: " <> r
