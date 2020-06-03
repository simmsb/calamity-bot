{-# LANGUAGE TypeApplications #-}

-- | Prefix models
module CalamityBot.Db.Prefixes
  ( addPrefix,
    getPrefixes,
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

getPrefixes :: Snowflake Guild -> Statement DB () (Only L.Text)
getPrefixes (fromIntegral @_ @Int64 . fromSnowflake -> gid) =
  query $
    select_
      (#p ! #pre `as` #fromOnly)
      ( from (table (#prefixes `as` #p))
          & where_ (#p ! #guild_id .== inline gid)
      )

removePrefix :: (Snowflake Guild, L.Text) -> Statement DB () ()
removePrefix (fromIntegral @_ @Int64 . fromSnowflake -> gid, pre) =
  manipulation $ deleteFrom_ #prefixes (#guild_id .== inline gid .&& #pre .== inline pre)
