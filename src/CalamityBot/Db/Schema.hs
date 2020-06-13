-- | DB schema
module CalamityBot.Db.Schema
  ( GuildsColumns,
    PrefixesColumns,
    RemindersColumns,
    Schema,
    DB,
  )
where

import Squeal.PostgreSQL

type GuildsColumns =
  '[ "id" ::: 'NoDef :=> 'NotNull 'PGint8,
     "last_seen" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
   ]

type GuildsConstraints = '["guilds_pkey" ::: 'PrimaryKey '["id"]]

type PrefixesColumns =
  '[ "guild_id" ::: 'NoDef :=> 'NotNull 'PGint8,
     "pre" ::: 'NoDef :=> 'NotNull 'PGtext
   ]

type PrefixesConstraints =
  '[ "prefixes_pkey" ::: 'PrimaryKey '["guild_id", "pre"],
     "prefixes_guild_id_fkey" ::: 'ForeignKey '["guild_id"] "guilds" '["id"]
   ]

type RemindersColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGuuid,
     "user_id" ::: 'NoDef :=> 'NotNull 'PGint8,
     "channel_id" ::: 'NoDef :=> 'NotNull 'PGint8,
     "message" ::: 'NoDef :=> 'NotNull 'PGtext,
     "created" ::: 'NoDef :=> 'NotNull 'PGtimestamptz,
     "target" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
   ]

type RemindersConstraints =
  '[ "reminders_pkey" ::: 'PrimaryKey '["id"]
   ]

type Schema =
  '[ "guilds" ::: 'Table (GuildsConstraints :=> GuildsColumns),
     "prefixes" ::: 'Table (PrefixesConstraints :=> PrefixesColumns),
     "reminders" ::: 'Table (RemindersConstraints :=> RemindersColumns)
   ]

type DB = Public Schema
