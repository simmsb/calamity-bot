-- |
module CalamityBot.Db.Eff
  ( DBEff (..),
    usingConn,
    runDBEffPooled,
  )
where

import Polysemy
import Squeal.PostgreSQL

data DBEff db m a where
  UsingConn :: PQ db db IO a -> DBEff db m a

makeSem ''DBEff

runDBEffPooled :: Member (Embed IO) r => Pool (K Connection db) -> Sem (DBEff db ': r) a -> Sem r a
runDBEffPooled pool = interpret \case UsingConn c -> embed @IO $ usingConnectionPool pool c
