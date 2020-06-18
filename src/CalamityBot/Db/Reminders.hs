-- | Reminder models
module CalamityBot.Db.Reminders
  ( addReminder,
    removeReminder,
    removeReminderByIdx,
    allRemindersFor,
    allRemindersForPaginated,
    upcomingReminders,
  )
where

import Calamity (Channel, Snowflake (..), User)
import CalamityBot.Db.Schema
import CalamityBot.Db.Utils ()
import Control.Lens hiding ((<.))
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import Database.Beam
import qualified Database.Beam.Postgres as Pg

addReminder :: (Snowflake User, Snowflake Channel, L.Text, UTCTime, UTCTime) -> SqlInsert Pg.Postgres DBReminderT
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

removeReminder :: DBReminder -> SqlDelete Pg.Postgres DBReminderT
removeReminder DBReminder {reminderId = id_} =
  delete (db ^. #reminders) (\r -> (r ^. #reminderId) ==. val_ id_)

removeReminderByIdx :: (Snowflake User, Int) -> SqlDelete Pg.Postgres DBReminderT
removeReminderByIdx (uid, idx) =
  delete
    (db ^. #reminders)
    ( \_ ->
        exists_
          $ filter_ (\row -> row ==. val_ idx)
          $ withWindow_
            (\r -> frame_ noPartition_ (orderPartitionBy_ . asc_ . reminderTarget $ r) noBounds_)
            (\_ w -> rowNumber_ `over_` w)
            (filter_ (\r -> reminderUserId r ==. val_ uid) $ all_ (db ^. #reminders))
    )

allRemindersFor :: Snowflake User -> Q Pg.Postgres BotDB s (DBReminderT (QGenExpr QValueContext Pg.Postgres s))
allRemindersFor uid =
  orderBy_ (\r -> asc_ $ reminderTarget r) $
    filter_ (\r -> reminderUserId r ==. val_ uid) (all_ $ db ^. #reminders)

allRemindersForPaginated :: (Snowflake User, Integer, Integer) -> SqlSelect Pg.Postgres (DBReminder, Int)
allRemindersForPaginated (uid, width, idx) =
  select
    $ offset_ (idx * width)
    $ limit_ width
    $ withWindow_ (\_ -> frame_ noPartition_ noOrder_ noBounds_)
                  (\r w -> (r, countAll_ `over_` w))
                  (allRemindersFor uid)

inNMinutes :: QGenExpr e Pg.Postgres s Int -> QGenExpr e Pg.Postgres s UTCTime
inNMinutes = customExpr_ innm
  where innm :: (Monoid a, IsString a) => a -> a
        innm offs = "(NOW() + INTERVAL '" <> offs <> " MINUTES')"

upcomingReminders :: SqlSelect Pg.Postgres DBReminder
upcomingReminders =
  select $
    filter_
      (\r -> reminderTarget r <. inNMinutes (val_ 1))
      (all_ $ db ^. #reminders)

