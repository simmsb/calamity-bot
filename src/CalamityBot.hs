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
import qualified Di
import DiPolysemy

runBot :: IO ()
runBot = do
  token <- toLText <$> getEnv "BOT_TOKEN"
  Di.new \di ->
    void . runFinal
      . embedToFinal
      . runCacheInMemoryNoMsg
      . runMetricsNoop
      . runDiToIO di
      . runBotIO (BotToken token)
      $ pure ()
