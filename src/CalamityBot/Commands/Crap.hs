{-# LANGUAGE BlockArguments #-}

-- | Crappy commands
module CalamityBot.Commands.Crap
  ( crapGroup,
  )
where

import Calamity
import Calamity.Commands
import CalamityBot.Commands.Reanimate.RenderInMem
import Control.Lens hiding (Context)
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as L
import Network.Mime
import qualified Polysemy as P
import Network.Wreq

findVideo :: [Attachment] -> Maybe Attachment
findVideo = listToMaybe . filter (\a -> "video" `B.isPrefixOf` defaultMimeLookup (toStrict $ a ^. #filename))

crapGroup :: BotC r => P.Sem (DSLState r) ()
crapGroup = void
  . help (const "Shitty commands that I Hate")
  . groupA "crap" ["c"]
  $ do
    help (const "Haha get stickbugged lol") $
      commandA @'[Named "delay (seconds)" (Maybe Float), Named "filename" (Maybe Text)]
        "stickbug"
        ["sb"]
        \ctx (fromMaybe 0.5 -> delay) (fromMaybe "get_stickbugged" -> fn) -> do
          case findVideo (ctx ^. #message . #attachments) of
            Just video -> do
              r <- P.embed $ Network.Wreq.get (L.unpack $ video ^. #url)
              let file = r ^. responseBody
              Just ext <- pure ((last <$>) . nonEmpty . fileNameExtensions . toStrict $ video ^. #filename)
              out <- P.embed $ renderStickbug (file, ext) delay
              case out of
                Right res ->
                  void $ tell ctx (TFile (fn <> ".mp4") (fromStrict res))
                Left e -> print e
            Nothing ->
              void $ tell @L.Text ctx "Couldn't find a video"
