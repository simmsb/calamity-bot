-- |
module CalamityBot.Utils.Config
  ( Config (..),
    getCfg,
    configAsConst,
  )
where

import Polysemy
import Data.HashMap.Strict ((!))

data Config m a where
  GetCfg :: Text -> Config m Text

makeSem ''Config

configAsConst :: HashMap Text Text -> Sem (Config ': r) a -> Sem r a
configAsConst cfg = interpret \case
  GetCfg k -> pure $ cfg ! k
