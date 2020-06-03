{-# LANGUAGE TypeApplications #-}

-- | Prefix models
module CalamityBot.Db.Prefixes
  ( addPrefix,
    removePrefix,
  )
where

import Calamity (Guild, Snowflake (..))
import qualified Data.Text.Lazy as L
import CalamityBot.Db.Schema
import Squeal.PostgreSQL

addPrefix :: (Snowflake Guild, L.Text) -> Statement DB () ()
addPrefix (fromIntegral @_ @Int64 . fromSnowflake -> gid, pre) =
  manipulation $
    insertInto_
      #prefixes
      (Values_ (Set (inline gid) `as` #guild_id :* Set (inline pre) `as` #pre))

removePrefix :: (Snowflake Guild, L.Text) -> Statement DB () ()
removePrefix (fromIntegral @_ @Int64 . fromSnowflake -> gid, pre) =
  manipulation $ deleteFrom_ #prefixes (#guild_id .== inline gid .&& #pre .== inline pre)
