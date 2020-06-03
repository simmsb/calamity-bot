{-# LANGUAGE TypeApplications #-}

-- | Guild models
module CalamityBot.Db.Guilds
  ( addGuild,
  )
where

import Calamity (Guild, Snowflake (..))
import CalamityBot.Db.Schema
import Squeal.PostgreSQL

addGuild :: Snowflake Guild -> Statement DB () ()
addGuild (fromIntegral @_ @Int64 . fromSnowflake -> gid) =
  manipulation $
    insertInto
      #guilds
      (Values_ (Set (inline gid) `as` #id :* Set now `as` #last_seen))
      ( OnConflict
          (OnConstraint #guilds_pkey)
          (DoUpdate (Set now `as` #last_seen) [])
      )
      (Returning_ Nil)
