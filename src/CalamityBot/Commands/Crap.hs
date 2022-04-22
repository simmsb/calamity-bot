-- | Crappy commands
module CalamityBot.Commands.Crap (
  crapGroup,
) where

import Calamity
import Calamity.Commands
import Calamity.Commands.Context (FullContext)
import CalamityBot.Utils.Config
import CalamityBot.Utils.Process
import CalamityBot.Utils.Utils
import Control.Lens hiding (Context)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Network.HTTP.Req as Req
import Network.Mime
import Numeric
import qualified Polysemy as P
import System.FilePath
import System.Process (callProcess)
import qualified Text.URI as URI

findVideo :: [Attachment] -> Maybe Attachment
findVideo = find (\a -> "video" `B.isPrefixOf` defaultMimeLookup (a ^. #filename))

crapGroup :: (BotC r, P.Member Config r) => P.Sem (DSLState FullContext r) ()
crapGroup = void
  . help (const "Shitty commands that I Hate")
  . groupA "crap" ["c"]
  $ do
    help (const "Haha get stickbugged lol") $
      commandA @'[Named "delay in seconds" (Maybe Float), Named "filename" (Maybe Text)]
        "stickbug"
        ["sb"]
        \ctx (fromMaybe 0.5 -> delay) (fromMaybe "get_stickbugged" -> fn) -> do
          sbfile <- getCfg "stickbug_path"
          case findVideo (ctx ^. #message . #attachments) of
            Just video -> do
              Just uri <- pure $ URI.mkURI (video ^. #url)
              Just (url, options) <- pure $ Req.useHttpsURI uri
              r <- P.embed . Req.runReq Req.defaultHttpConfig $ Req.req Req.GET url Req.NoReqBody Req.lbsResponse options
              let file = Req.responseBody r
              Just ext <- pure ((last <$>) . nonEmpty . fileNameExtensions $ video ^. #filename)
              out <- P.embed $ renderStickbug (file, ext) sbfile delay
              case out of
                Right res ->
                  void $ tell ctx (CreateMessageAttachment (fn <> ".mp4") Nothing res)
                Left e -> putLBSLn e
            Nothing ->
              void $ tell @T.Text ctx "Couldn't find a video"

    -- shameless
    help (const "Haha get bunnyd lol") $
      commandA @'[Named "delay in seconds" (Maybe Float), Named "filename" (Maybe Text)]
        "bunny"
        ["bn"]
        \ctx (fromMaybe 0.5 -> delay) (fromMaybe "get_bunnyd" -> fn) -> do
          sbfile <- getCfg "bunny_path"
          case findVideo (ctx ^. #message . #attachments) of
            Just video -> do
              Just uri <- pure $ URI.mkURI (video ^. #url)
              Just (url, options) <- pure $ Req.useHttpsURI uri
              r <- P.embed . Req.runReq Req.defaultHttpConfig $ Req.req Req.GET url Req.NoReqBody Req.lbsResponse options
              let file = Req.responseBody r
              Just ext <- pure ((last <$>) . nonEmpty . fileNameExtensions $ video ^. #filename)
              out <- P.embed $ renderStickbug (file, ext) sbfile delay
              case out of
                Right res ->
                  void $ tell ctx (CreateMessageAttachment (fn <> ".mp4") Nothing res)
                Left e -> putLBSLn e
            Nothing ->
              void $ tell @T.Text ctx "Couldn't find a video"

    help (const "We are japanese goblin") $
      command @'[Named "delay in seconds" (Maybe Float), Named "filename" (Maybe Text)]
        "goblin"
        \ctx (fromMaybe 0.5 -> delay) (fromMaybe "we_are_japanese_goblin" -> fn) -> do
          sbfile <- getCfg "goblin_path"
          case findVideo (ctx ^. #message . #attachments) of
            Just video -> do
              Just uri <- pure $ URI.mkURI (video ^. #url)
              Just (url, options) <- pure $ Req.useHttpsURI uri
              r <- P.embed . Req.runReq Req.defaultHttpConfig $ Req.req Req.GET url Req.NoReqBody Req.lbsResponse options
              let file = Req.responseBody r
              Just ext <- pure ((last <$>) . nonEmpty . fileNameExtensions $ video ^. #filename)
              out <- P.embed $ renderGoblin (file, ext) sbfile delay
              case out of
                Right res ->
                  void $ tell ctx (CreateMessageAttachment (fn <> ".mp4") Nothing res)
                Left e -> putLBSLn e
            Nothing ->
              void $ tell @T.Text ctx "Couldn't find a video"

{- ORMOLU_DISABLE -}

renderGoblin ::
  (LB.ByteString, Text) ->
  Text ->
  Float ->
  IO (Either LB.ByteString LB.ByteString)
renderGoblin (initial, ext) sbfile delay = do
  ffmpeg <- requireExecutable "ffmpeg"
  withTempFile "stickbug" (T.unpack ext) $ \initialFile -> do
    writeFileLBS initialFile initial
    let df = showFFloat Nothing delay ""
    runCmdLazy
      ffmpeg
      [ "-i"
      , initialFile
      , "-i"
      , T.unpack sbfile
      , "-threads"
      , "0"
      , "-filter_complex"
      , "[0]scale=800:800[s0];"
         <> "[s0]setsar=1/1[s1];"
         <> "[s1]split=2[s2][s3];"
         <> "[s2]trim=end=" <> df <> "[s4];"
         <> "[s3]trim=duration=0.092:start=" <> df <> "[s5];"
         <> "[s5]setpts=PTS-STARTPTS[s6];"
         <> "[s6]split=4[s7][s8][s9][s10];"
         <> "[s7]reverse[s11];"
         <> "[s11]setpts=PTS-STARTPTS[s12];"
         <> "[s12]split=3[s13][s14][s15];"
         <> "[s8]setpts=PTS*0.538[s16];"
         <> "[s13]setpts=PTS*0.538[s17];"
         <> "[s9][s14][s16][s15][s10][s17]concat=n=6[s18];"
         <> "[1]scale=800:800[s19];"
         <> "[s19]setsar=1/1[s20];"
         <> "[s20]split=1[s21];"
         <> "[s21]trim=start=0.71[s22];"
         <> "[s22]setpts=PTS-STARTPTS[s23];"
         <> "[s4][s18][s23]concat=a=0:n=3:v=1[s24];"
         <> "[0:a]atrim=end=" <> df <> "[s25];"
         <> "[1:a]atrim=end=0.71[s26];"
         <> "[s26]asetpts=PTS-STARTPTS[s27];"
         <> "[1:a]atrim=start=0.71[s28];"
         <> "[s28]asetpts=PTS-STARTPTS[s29];"
         <> "[s25][s27][s29]concat=a=1:n=3:v=0[s30]"
      , "-map", "[s24]", "-map", "[s30]"
      , "-c:v", "libx264"
      , "-c:a", "aac"
      , "-r", "30"
      , "-preset", "veryfast"
      , "-movflags"
      , "+faststart"
      , "-pix_fmt"
      , "yuv420p"
      , "-f"
      , "ismv"
      , "-"
      ]

renderStickbug ::
  (LB.ByteString, Text) ->
  Text ->
  Float ->
  IO (Either LB.ByteString LB.ByteString)
renderStickbug (initial, ext) sbfile delay = do
  ffmpeg <- requireExecutable "ffmpeg"
  withTempFile "stickbug" (T.unpack ext) $ \initialFile -> do
    writeFileLBS initialFile initial
    let df = showFFloat Nothing delay ""
    runCmdLazy
      ffmpeg
      [ "-i"
      , initialFile
      , "-i"
      , T.unpack sbfile
      , "-threads"
      , "0"
      , "-filter_complex"
      , "[1:v][0:v]scale2ref[v1][v0];"
          <> "[v0]trim=end="
          <> df
          <> ", setpts=PTS-STARTPTS,setsar=sar=1[v00];"
          <> "[v1]setsar=sar=1[v01];"
          <> "[0:a]atrim=end="
          <> df
          <> ", asetpts=PTS-STARTPTS[a0];"
          <> "[1:a]dynaudnorm, volume=3, asetpts=PTS-STARTPTS[a1]; "
          <> "[v00][a0][v01][a1]concat=n=2:v=1:a=1[v][a]"
      , "-map"
      , "[v]"
      , "-map"
      , "[a]"
      , "-c:v"
      , "libx264"
      , "-c:a"
      , "aac"
      , "-r"
      , "30"
      , "-crf"
      , "18"
      , "-movflags"
      , "+faststart"
      , "-pix_fmt"
      , "yuv420p"
      , "-f"
      , "ismv"
      , "-"
      ]
