-- | Reminder models
module CalamityBot.Db.Reminders (
  addReminder,
  removeReminder,
  allRemindersFor,
  remindersForPaginatedInitial,
  remindersForPaginatedBefore,
  remindersForPaginatedAfter,
  upcomingReminders,
) where

import Calamity (Channel, Snowflake (..), User)
import CalamityBot.Db.Schema
import CalamityBot.Db.Utils
import Data.Text qualified as T
import Data.Time.Clock
import Database.Beam
import Database.Beam.Postgres qualified as Pg
import Optics
import GHC.Exts (IsString)

addReminder :: (Snowflake User, Snowflake Channel, T.Text, UTCTime, UTCTime) -> SqlInsert Pg.Postgres DBReminderT
addReminder (uid, cid, msg, created, target) =
  insert
    (db ^. #reminders)
    ( insertExpressions
        [ DBReminder
            default_
            (val_ uid)
            (val_ cid)
            (val_ msg)
            (val_ created)
            (val_ target)
        ]
    )

removeReminder :: (Snowflake User, T.Text) -> SqlDelete Pg.Postgres DBReminderT
removeReminder (uid, rid) =
  delete
    (db ^. #reminders)
    (\r -> (reminderId r ==. val_ rid) &&. (reminderUserId r ==. val_ uid))

allRemindersFor :: Snowflake User -> Q Pg.Postgres BotDB s (DBReminderT (QGenExpr QValueContext Pg.Postgres s))
allRemindersFor uid =
  orderBy_ (\r -> (asc_ $ reminderTarget r, asc_ $ reminderId r)) $
    filter_ (\r -> reminderUserId r ==. val_ uid) (all_ $ db ^. #reminders)

allRemindersForR :: Snowflake User -> Q Pg.Postgres BotDB s (DBReminderT (QGenExpr QValueContext Pg.Postgres s))
allRemindersForR uid =
  orderBy_ (\r -> (desc_ $ reminderTarget r, desc_ $ reminderId r)) $
    filter_ (\r -> reminderUserId r ==. val_ uid) (all_ $ db ^. #reminders)

remindersForPaginatedInitial :: (Snowflake User, Int) -> SqlSelect Pg.Postgres DBReminder
remindersForPaginatedInitial (uid, width) =
  select $
    limit_ (fromIntegral width) $
      allRemindersFor uid

remindersForPaginatedBefore :: (Snowflake User, Int, UTCTime, T.Text) -> SqlSelect Pg.Postgres DBReminder
remindersForPaginatedBefore (uid, width, target, rid) =
  select $
    limit_ (fromIntegral width) $
      filter_ (\r -> (reminderTarget r, reminderId r) `tupleLT` (val_ target, val_ rid)) $
        allRemindersForR uid

remindersForPaginatedAfter :: (Snowflake User, Int, UTCTime, T.Text) -> SqlSelect Pg.Postgres DBReminder
remindersForPaginatedAfter (uid, width, target, rid) =
  select $
    limit_ (fromIntegral width) $
      filter_ (\r -> (reminderTarget r, reminderId r) `tupleGT` (val_ target, val_ rid)) $
        allRemindersFor uid

inNMinutes :: QGenExpr e Pg.Postgres s Integer -> QGenExpr e Pg.Postgres s UTCTime
inNMinutes = customExpr_ innm
  where
    innm :: (Monoid a, IsString a) => a -> a
    innm offs = "(NOW() + INTERVAL '" <> offs <> " MINUTES')"

upcomingReminders :: SqlSelect Pg.Postgres DBReminder
upcomingReminders =
  select $
    filter_
      (\r -> reminderTarget r <. inNMinutes (val_ 1))
      (all_ $ db ^. #reminders)
