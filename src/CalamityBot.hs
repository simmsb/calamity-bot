{-# LANGUAGE QuasiQuotes #-}

module CalamityBot
  ( runBot,
  )
where

import Calamity
import Calamity.Cache.InMemory
import System.Environment
import Polysemy
import Calamity.Metrics.Noop (runMetricsNoop)

runBot :: IO ()
runBot = do
  token <- toLText <$> getEnv "BOT_TOKEN"
  void . runFinal
    . embedToFinal
    . runCacheInMemoryNoMsg
    . runMetricsNoop
    . runBotIO (BotToken token)
    $ pure ()
