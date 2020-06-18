-- | DB schema
module CalamityBot.Db.Schema
  ( DBGuildT (..),
    DBPrefixT (..),
    DBReminderT (..),
    DBGuild,
    DBPrefix,
    DBReminder,
    BotDB,
    PrimaryKey(DBGuildId, DBPrefixId, DBReminderId),
    db,
  )
where

-- https://haskell-beam.github.io/beam/user-guide/models/#the-beamable-type-class

import Calamity
import Data.Time (UTCTime)
import Data.UUID.Types
import Database.Beam

data DBGuildT f = DBGuild
  { guildId       :: Columnar f (Snowflake Guild),
    guildLastSeen :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

type DBGuild = DBGuildT Identity

instance Table DBGuildT where
  data PrimaryKey DBGuildT f = DBGuildId (Columnar f (Snowflake Guild)) deriving (Generic, Beamable)
  primaryKey = DBGuildId . guildId

data DBPrefixT f = DBPrefix
  { prefixGuild :: PrimaryKey DBGuildT f,
    prefixPre   :: Columnar f LText
  }
  deriving (Generic, Beamable)

type DBPrefix = DBPrefixT Identity

instance Table DBPrefixT where
  data PrimaryKey DBPrefixT f = DBPrefixId (PrimaryKey DBGuildT f) (Columnar f LText) deriving (Generic, Beamable)
  primaryKey = DBPrefixId <$> prefixGuild <*> prefixPre

data DBReminderT f = DBReminder
  { reminderId        :: Columnar f UUID,
    reminderUserId    :: Columnar f (Snowflake User),
    reminderChannelId :: Columnar f (Snowflake Channel),
    reminderMessage   :: Columnar f LText,
    reminderCreated   :: Columnar f UTCTime,
    reminderTarget    :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

type DBReminder = DBReminderT Identity

instance Table DBReminderT where
  data PrimaryKey DBReminderT f = DBReminderId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = DBReminderId . reminderId

data BotDB f = BotDB
  { guilds    :: f (TableEntity DBGuildT),
    prefixes  :: f (TableEntity DBPrefixT),
    reminders :: f (TableEntity DBReminderT)
  }
  deriving (Generic, Database be)

db :: DatabaseSettings be BotDB
db = defaultDbSettings
