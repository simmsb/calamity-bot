{-# OPTIONS_GHC -Wno-orphans #-}

-- | db utilities
module CalamityBot.Db.Utils
  ( idAsInt,
  )
where

import Calamity (Snowflake (..))
import Squeal.PostgreSQL

idAsInt :: Snowflake a -> Int64
idAsInt = fromIntegral . fromSnowflake

instance Inline (Snowflake a) where
  inline = coerce . inline . idAsInt

instance IsPG (Snowflake a) where
  type PG (Snowflake a) = PG Int64

instance ToPG db (Snowflake a) where
  toPG = toPG . idAsInt

instance FromPG (Snowflake a) where
  fromPG = Snowflake . fromIntegral @Int64 @Word64 <$> fromPG
