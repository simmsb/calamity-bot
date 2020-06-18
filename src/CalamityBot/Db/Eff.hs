-- |
module CalamityBot.Db.Eff
  ( DBEff (..),
    usingConn,
    runDBEffPooled,
  )
where

import Polysemy
import Data.Pool
import Database.PostgreSQL.Simple
import Database.Beam.Postgres (runBeamPostgres, Pg)

data DBEff m a where
  UsingConn :: Pg a -> DBEff m a

makeSem ''DBEff

runDBEffPooled :: forall r a. Member (Embed IO) r => Pool Connection -> Sem (DBEff ': r) a -> Sem r a
runDBEffPooled pool = interpret \case UsingConn m -> embed $ withResource pool (flip runBeamPostgres m)
