-- | Guild models
module CalamityBot.Db.Guilds (
  addGuild,
) where

import Calamity (Guild, Snowflake (..))
import CalamityBot.Db.Schema
import CalamityBot.Db.Utils ()
import Database.Beam
import Database.Beam.Postgres qualified as Pg
import Database.Beam.Postgres.Full qualified as Pg
import Optics

addGuild :: Snowflake Guild -> SqlInsert Pg.Postgres DBGuildT
addGuild g =
  Pg.insert (db ^. #guilds) (insertExpressions [DBGuild (val_ g) default_]) $
    Pg.onConflict
      (Pg.conflictingFields primaryKey)
      (Pg.onConflictUpdateInstead (^. #guildLastSeen))
