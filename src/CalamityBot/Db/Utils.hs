{-# OPTIONS_GHC -Wno-orphans #-}

-- | db utilities
module CalamityBot.Db.Utils
  ( idAsInt,
    tupleLT,
    tupleGT,
  )
where

import Calamity (Snowflake (..))
import Database.Beam (FromBackendRow, HasSqlEqualityCheck (..), QGenExpr, SqlEq, SqlOrd, (&&.), (<.), (==.), (>.), (||.))
import Database.Beam.Backend (BeamSqlBackend, HasSqlValueSyntax (..))
import Database.Beam.Backend.SQL.AST (Value)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)

idAsInt :: Snowflake a -> Int64
idAsInt = fromIntegral . fromSnowflake


deriving via (Word64) instance HasSqlValueSyntax Value (Snowflake a)
deriving via (Word64) instance HasSqlValueSyntax PgValueSyntax (Snowflake a)
deriving via (Word64) instance FromBackendRow Postgres (Snowflake a)


instance HasSqlEqualityCheck Postgres (Snowflake a)
-- instance SqlOrderable Postgres LText


-- instance HasSqlValueSyntax Value (Snowflake a) where
--   sqlValueSyntax = sqlValueSyntax . fromSnowflake

-- instance Inline (Snowflake a) where
--   inline = coerce . inline . idAsInt

-- instance IsPG (Snowflake a) where
--   type PG (Snowflake a) = PG Int64

-- instance ToPG db (Snowflake a) where
--   toPG = toPG . idAsInt

-- instance FromPG (Snowflake a) where
--   fromPG = Snowflake . fromIntegral @Int64 @Word64 <$> fromPG

tupleLT :: (BeamSqlBackend be, SqlOrd (QGenExpr context be s) a2, SqlEq (QGenExpr context be s) a1, SqlOrd (QGenExpr context be s) a1) => (a1, a2) -> (a1, a2) -> QGenExpr context be s Bool
tupleLT (a, b) (x, y) = (a <. x) ||. (a ==. x &&. b <. y)

tupleGT :: (BeamSqlBackend be, SqlOrd (QGenExpr context be s) a2, SqlEq (QGenExpr context be s) a1, SqlOrd (QGenExpr context be s) a1) => (a1, a2) -> (a1, a2) -> QGenExpr context be s Bool
tupleGT (a, b) (x, y) = (a >. x) ||. (a ==. x &&. b >. y)
