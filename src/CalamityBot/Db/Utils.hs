{-# OPTIONS_GHC -Wno-orphans #-}

-- | db utilities
module CalamityBot.Db.Utils
  ( idAsInt,
  )
where

import Calamity (Snowflake (..))
import Database.Beam.Backend (HasSqlValueSyntax(..))
import Database.Beam.Backend.SQL.AST (Value)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.Beam (HasSqlEqualityCheck(..))
import Database.Beam.Postgres (Postgres)
import Database.Beam (FromBackendRow)

idAsInt :: Snowflake a -> Int64
idAsInt = fromIntegral . fromSnowflake

deriving via (Word64) instance HasSqlValueSyntax Value (Snowflake a)
deriving via (Word64) instance HasSqlValueSyntax PgValueSyntax (Snowflake a)
deriving via (Word64) instance  FromBackendRow Postgres (Snowflake a)

instance HasSqlEqualityCheck Postgres (Snowflake a)

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
