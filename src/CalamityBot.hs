{-# LANGUAGE QuasiQuotes #-}

module CalamityBot
  ( runBot,
  )
where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Gateway.Types (StatusUpdateData (..))
import Calamity.Metrics.Noop
import CalamityBot.Commands
import CalamityBot.Db
import CalamityBot.PrefixHandler
import CalamityBot.Utils.Config
import Data.Pool (createPool)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as L
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import DiPolysemy
import Polysemy
import Polysemy.Immortal
import Polysemy.Timeout
import System.Environment
import TextShow

cfg :: HashMap Text Text
cfg =
  fromList
    [ ("stickbug_path", "assets/stickbug.mp4")
    , ("bunny_path", "assets/bunny.mp4")
    ]

runBot :: IO ()
runBot = do
  token <- L.pack <$> getEnv "BOT_TOKEN"
  db_path <- BS.pack <$> getEnv "DB_STRING"
  pool <- createPool (connectPostgreSQL db_path) close 3 0.5 30
  void . runFinal
    . embedToFinal
    . configAsConst cfg
    . timeoutToIOFinal
    . immortalToIOFinal
    . runDBEffPooled pool
    . runCacheInMemoryNoMsg
    . runMetricsNoop
    . useDatabasePrefix "c!"
    . runBotIO (BotToken token)
    $ do
      addCommands do
        void helpCommand
        prefixGroup
        reminderGroup
        aliasGroup
        reanimateGroup
        crapGroup
        hide do
          group "cantseethis" do
            command @'[] "nope" \ctx ->
              void $ tell @L.Text ctx "You found me"
          command @'[] "cantseeme" \ctx ->
            void $ tell @L.Text ctx "You found me"
      react @('CustomEvt "command-error" (Context, CommandError)) \(ctx, e) -> do
        info $ "Command failed with reason: " <> showtl e
        case e of
          ParseError n r ->
            void . tell ctx $
              "Failed to parse parameter: " <> codeline (L.fromStrict n)
                <> ", with reason: "
                <> codeblock' Nothing r
          CheckError n r ->
            void . tell ctx $
              "The following check failed: " <> codeline (L.fromStrict n)
                <> ", with reason: "
                <> codeblock' Nothing r
          InvokeError n r ->
            void . tell ctx $
              "The command: " <> codeline (L.fromStrict n)
                <> ", failed with reason: "
                <> codeblock' Nothing r
      react @'ReadyEvt \_ -> do
        sendPresence
          StatusUpdateData
            { since = Nothing,
              game = Just $ activity "Prefix: c!" Game,
              status = "online",
              afk = False
            }
      pure ()
