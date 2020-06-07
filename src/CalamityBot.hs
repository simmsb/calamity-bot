module CalamityBot
  ( runBot,
  )
where

import Calamity (Token (..), runBotIO)
import Calamity.Commands
import Calamity.Cache.InMemory
import Calamity.Metrics.Noop
import CalamityBot.Commands
import CalamityBot.Db
import CalamityBot.PrefixHandler
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as L
import Polysemy
import Squeal.PostgreSQL
import System.Environment

runBot :: IO ()
runBot = do
  token <- L.pack <$> getEnv "BOT_TOKEN"
  db_path <- BS.pack <$> getEnv "DB_STRING"
  pool <- createConnectionPool db_path 3 0.5 10
  void . runFinal . embedToFinal . runDBEffPooled pool . runCacheInMemoryNoMsg . runMetricsNoop . useDatabasePrefix "c!" . runBotIO (BotToken token) $ addCommands do
    void helpCommand
    prefixGroup
    pure ()
