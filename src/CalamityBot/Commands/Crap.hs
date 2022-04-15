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
import Network.Mime
import qualified Network.HTTP.Req as Req
import qualified Text.URI as URI
import Numeric
import qualified Polysemy as P

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
