{-# LANGUAGE QuasiQuotes #-}

module CalamityBot
  ( runBot,
  )
where

import Calamity
import Calamity.Commands
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
      . runDiToIO di
      . useConstantPrefix "leakcheck!"
      . runCacheInMemoryNoMsg
      . runMetricsNoop
      . runBotIO (BotToken token)
      $ addCommands $ do
        helpCommand
        command @'[] "test" \ctx ->
          void $ tell @Text ctx "hi"
