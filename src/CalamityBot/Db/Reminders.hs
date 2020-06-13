-- | Reminder models
module CalamityBot.Db.Reminders
  ( addReminder,
    removeReminder,
    removeReminderByIdx,
    allRemindersFor,
    upcomingReminders,
  )
where

import Calamity (Channel, Snowflake (..), User)
import CalamityBot.Db.Schema
import CalamityBot.Db.Utils ()
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import Data.UUID.Types
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL

data Reminder = Reminder
  { id :: UUID,
    userID :: Snowflake User,
    channelID :: Snowflake Channel,
    message :: L.Text,
    created :: UTCTime,
    target :: UTCTime
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

addReminder :: (Snowflake User, Snowflake Channel, L.Text, UTCTime) -> Statement DB () Reminder
addReminder (uid, cid, msg, target) =
  manipulation $
    insertInto
      #reminders
      ( Values_
          ( Default `as` #id
              :* Set (inline uid) `as` #user_id
              :* Set (inline cid) `as` #channel_id
              :* Set (inline msg) `as` #message
              :* Set now `as` #created
              :* Set (inline target) `as` #target
          )
      )
      OnConflictDoRaise
      ( Returning_
          ( #id
              :* #user_id `as` #userID
              :* #channel_id `as` #channelID
              :* #message
              :* #created
              :* #target
          )
      )

removeReminder :: Reminder -> Statement DB () ()
removeReminder Reminder {id = id_} =
  manipulation $
    deleteFrom_
      #reminders
      (#id .== inline id_)

-- yuck
removeReminderByIdx :: (Snowflake User, Int64) -> Statement DB () ()
removeReminderByIdx (uid, idx) =
  manipulation $
    deleteFrom_
      #reminders
      ( subAny
          #id
          (.==)
          ( select_
              (#s0 ! #id)
              ( from
                  ( subquery
                      ( select
                          ((#r ! #id) & Also (rowNumber `as` #row_num `Over` (partitionBy (#r ! #id) & orderBy [#r ! #target & Asc])))
                          ( from (table (#reminders `as` #r))
                              & where_ (#r ! #user_id .== inline uid)
                          )
                          `As` #s0
                      )
                  )
                  & where_ (#s0 ! #row_num .== inline idx)
              )
          )
      )

allRemindersFor :: Snowflake User -> Statement DB () Reminder
allRemindersFor uid =
  query $
    select_
      ( #id
          :* #user_id `as` #userID
          :* #channel_id `as` #channelID
          :* #message
          :* #created
          :* #target
      )
      ( from (table #reminders)
          & where_ (#user_id .== inline uid)
          & orderBy [#target & Asc]
      )

upcomingReminders :: Statement DB () Reminder
upcomingReminders =
  query $
    select_
      ( #id
          :* #user_id `as` #userID
          :* #channel_id `as` #channelID
          :* #message
          :* #created
          :* #target
      )
      ( from (table #reminders)
          & where_ (#target .< now !+ interval_ 1 Minutes)
      )
