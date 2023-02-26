-- | Alias models
module CalamityBot.Db.Aliases (
  addAlias,
  getAlias,
  removeAliasByName,
  allAliasesFor,
  aliasesForPaginatedInitial,
  aliasesForPaginatedBefore,
  aliasesForPaginatedAfter,
) where

import Calamity (Snowflake (..), User)
import CalamityBot.Db.Schema
import CalamityBot.Db.Utils ()
import Data.Text qualified as T
import Database.Beam
import Database.Beam.Postgres qualified as Pg
import Optics

addAlias :: (Snowflake User, T.Text, T.Text) -> SqlInsert Pg.Postgres DBAliasT
addAlias (uid, name, value) =
  insert
    (db ^. #aliases)
    (insertValues [DBAlias uid name value])

getAlias :: (Snowflake User, T.Text) -> SqlSelect Pg.Postgres DBAlias
getAlias (uid, name) =
  select $
    filter_
      (\r -> (r ^. #aliasUserId) ==. val_ uid &&. (r ^. #aliasName) ==. val_ name)
      (all_ $ db ^. #aliases)

removeAliasByName :: (Snowflake User, T.Text) -> SqlDelete Pg.Postgres DBAliasT
removeAliasByName (uid, name) =
  delete (db ^. #aliases) (\r -> (r ^. #aliasUserId) ==. val_ uid &&. (r ^. #aliasName) ==. val_ name)

allAliasesFor :: Snowflake User -> Q Pg.Postgres BotDB s (DBAliasT (QGenExpr QValueContext Pg.Postgres s))
allAliasesFor uid =
  orderBy_ (\r -> asc_ $ r ^. #aliasName) $
    filter_
      (\r -> aliasUserId r ==. val_ uid)
      (all_ $ db ^. #aliases)

allAliasesForR :: Snowflake User -> Q Pg.Postgres BotDB s (DBAliasT (QGenExpr QValueContext Pg.Postgres s))
allAliasesForR uid =
  orderBy_ (\r -> desc_ $ r ^. #aliasName) $
    filter_
      (\r -> aliasUserId r ==. val_ uid)
      (all_ $ db ^. #aliases)

aliasesForPaginatedInitial :: (Snowflake User, Int) -> SqlSelect Pg.Postgres DBAlias
aliasesForPaginatedInitial (uid, width) =
  select $
    limit_ (fromIntegral width) $
      allAliasesFor uid

aliasesForPaginatedBefore :: (Snowflake User, Int, T.Text) -> SqlSelect Pg.Postgres DBAlias
aliasesForPaginatedBefore (uid, width, name) =
  select $
    limit_ (fromIntegral width) $
      filter_ (\r -> aliasName r <. val_ name) $
        allAliasesForR uid

aliasesForPaginatedAfter :: (Snowflake User, Int, T.Text) -> SqlSelect Pg.Postgres DBAlias
aliasesForPaginatedAfter (uid, width, name) =
  select $
    limit_ (fromIntegral width) $
      filter_ (\r -> aliasName r >. val_ name) $
        allAliasesFor uid
