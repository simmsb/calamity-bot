{-# LANGUAGE TypeApplications #-}

-- | Guild models
module CalamityBot.Db.Guilds
  ( addGuild,
  )
where

import Calamity (Guild, Snowflake (..))
import CalamityBot.Db.Schema
import CalamityBot.Db.Utils ()
import Control.Lens
import Database.Beam
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Postgres as Pg

addGuild :: Snowflake Guild -> SqlInsert Pg.Postgres DBGuildT
addGuild g =
  Pg.insert (db ^. #guilds) (insertExpressions [DBGuild (val_ g) default_]) $
    Pg.onConflict
      (Pg.conflictingFields primaryKey)
      (Pg.onConflictUpdateInstead (^. #guildLastSeen))
