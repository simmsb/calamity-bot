{-# LANGUAGE BlockArguments #-}

-- | Prefix related commands
module CalamityBot.Commands.Prefix
  ( prefixGroup,
  )
where

import Calamity.Commands
import Calamity
import CalamityBot.Db
import Control.Lens hiding (Context)
import qualified Data.Text.Lazy as L
import qualified Polysemy as P
import Relude.Unsafe (fromJust)
import Squeal.PostgreSQL (MonadPQ (execute), firstRow, fromOnly, getRows)
import TextShow (TextShow (showtl))

guildOnly :: Context -> Maybe L.Text
guildOnly ctx = maybe (Just "Can only be used in guilds") (const Nothing) (ctx ^. #guild)

prefixLimit :: P.Member (DBEff DB) r => Int -> Context -> P.Sem r (Maybe L.Text)
prefixLimit limit ctx =
  case ctx ^. #guild of
    Just g -> do
      np <- usingConn (execute (countPrefixes $ getID @Guild g) >>= (fromMaybe 0 . fmap fromOnly <$>) . firstRow)
      pure
        if np > fromIntegral limit
          then Just ("Prefix limit reached (" <> showtl limit <> ")")
          else Nothing
    Nothing -> pure Nothing

maintainGuild :: P.Member (DBEff DB) r => Snowflake Guild -> P.Sem r ()
maintainGuild gid = void $ usingConn (execute $ addGuild gid)

prefixGroup :: (BotC r, P.Member (DBEff DB) r) => P.Sem (DSLState r) ()
prefixGroup = void
  . help (const "Commands related to setting prefixes for the bot")
  . group "prefix"
  . requiresPure [("guildOnly", guildOnly)]
  $ do
    react @'GuildCreateEvt \(g, _) ->
      maintainGuild (getID g)

    requires' "prefixLimit" (prefixLimit 6) $ help (const "Add a new prefix") $
      command @'[Named "prefix" L.Text] "add" \ctx p -> do
        let g = fromJust (ctx ^. #guild)
            gid = getID @Guild g
        maintainGuild gid
        usingConn (execute $ addPrefix (gid, p))
        void $ tell @L.Text ctx ("Added prefix: " <> p)

    help (const "Remove a prefix") $
      command @'[Named "prefix" L.Text] "remove" \ctx p -> do
        let g = fromJust (ctx ^. #guild)
        usingConn (execute $ removePrefix (getID @Guild g, p))
        void $ tell @L.Text ctx ("Removed prefix (if it existed): " <> p)

    help (const "List prefixes") $
      command @'[] "list" \ctx -> do
        let g = fromJust (ctx ^. #guild)
            gid = getID @Guild g
        prefixes <- usingConn (execute (getPrefixes gid) >>= (fmap fromOnly <$>) . getRows)
        void $ tell @L.Text ctx ("Prefixes: " <> L.unwords ["`" <> p <> "`" | p <- prefixes])
