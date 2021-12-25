-- | Prefix models
module CalamityBot.Db.Prefixes
  ( addPrefix,
    getPrefixes,
    getPrefixes',
    countPrefixes,
    removePrefix,
  )
where

import Calamity (Guild, Snowflake (..))
import CalamityBot.Db.Schema
import CalamityBot.Db.Utils ()
import Control.Lens
import qualified Data.Text as T
import Database.Beam
import qualified Database.Beam.Postgres as Pg

addPrefix :: (Snowflake Guild, T.Text) -> SqlInsert Pg.Postgres DBPrefixT
addPrefix (gid, pre_) =
  insert (db ^. #prefixes) (insertValues [DBPrefix (DBGuildId gid) pre_])

countPrefixes :: Snowflake Guild -> SqlSelect Pg.Postgres Integer
countPrefixes gid = select $
  aggregate_
    (const countAll_)
    (filter_ (\p -> prefixGuild p ==. val_ (DBGuildId gid)) $ all_ (db ^. #prefixes))

getPrefixes' :: Snowflake Guild -> SqlSelect Pg.Postgres T.Text
getPrefixes' = select . getPrefixes

getPrefixes :: Snowflake Guild -> Q Pg.Postgres BotDB s (QGenExpr QValueContext Pg.Postgres s T.Text)
getPrefixes gid = do
  ps <- filter_ (\p -> prefixGuild p ==. val_ (DBGuildId gid)) $ all_ (db ^. #prefixes)
  pure $ ps ^. #prefixPre

removePrefix :: (Snowflake Guild, T.Text) -> SqlDelete Pg.Postgres DBPrefixT
removePrefix (gid, pre_) =
  delete (db ^. #prefixes) (\p -> (prefixGuild p ==. val_ (DBGuildId gid)) &&. ((p ^. #prefixPre) ==. val_ pre_))
