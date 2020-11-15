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
      . useConstantPrefix "leakcheck!"
      . runCacheInMemoryNoMsg
      . runMetricsNoop
      . runDiToIO di
      . runBotIO (BotToken token)
      $ addCommands $ do
        helpCommand
        forM_ [0 .. 100] \i ->
          command @'[] ("test" <> show i) \ctx ->
            void $ tell @Text ctx "hi"
