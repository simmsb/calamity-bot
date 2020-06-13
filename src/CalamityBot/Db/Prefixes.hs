-- | Prefix models
module CalamityBot.Db.Prefixes
  ( addPrefix,
    getPrefixes,
    countPrefixes,
    removePrefix,
  )
where

import Calamity (Guild, Snowflake (..))
import qualified Data.Text.Lazy as L
import CalamityBot.Db.Schema
import CalamityBot.Db.Utils ()
import Squeal.PostgreSQL

addPrefix :: (Snowflake Guild, L.Text) -> Statement DB () ()
addPrefix (gid, pre) =
  manipulation $
    insertInto_
      #prefixes
      (Values_ (Set (inline gid) `as` #guild_id :* Set (inline pre) `as` #pre))

countPrefixes :: Snowflake Guild -> Statement DB () (Only Int64)
countPrefixes gid =
  query $
    select_
      (countStar `as` #fromOnly)
      ( from (table #prefixes)
          & where_ (#guild_id .== inline gid)
          & groupBy #guild_id
      )

getPrefixes :: Snowflake Guild -> Statement DB () (Only L.Text)
getPrefixes gid =
  query $
    select_
      (#pre `as` #fromOnly)
      ( from (table #prefixes)
          & where_ (#guild_id .== inline gid)
      )

removePrefix :: (Snowflake Guild, L.Text) -> Statement DB () ()
removePrefix (gid, pre) =
  manipulation $ deleteFrom_ #prefixes (#guild_id .== inline gid .&& #pre .== inline pre)
