-- | DB schema
module CalamityBot.Db.Schema
  ( GuildsColumns,
    PrefixesColumns,
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

type Schema =
  '[ "guilds" ::: 'Table (GuildsConstraints :=> GuildsColumns),
     "prefixes" ::: 'Table (PrefixesConstraints :=> PrefixesColumns)
   ]

type DB = Public Schema
