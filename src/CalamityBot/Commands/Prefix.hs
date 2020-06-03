-- | Prefix related commands
module CalamityBot.Commands.Prefix
  ( prefixGroup,
  )
where

import Calamity.Commands
import Calamity
import CalamityBot.Db
import qualified Data.Text.Lazy as L
import Control.Lens
import Squeal.PostgreSQL (MonadPQ (execute))
import qualified Polysemy as P

prefixGroup :: (BotC r, P.Member (DBEff DB) r) => P.Sem (DSLState r) ()
prefixGroup = void . group "prefix" $ do
  help (const "Add a new prefix")
    $ command @'[Named "prefix" L.Text] "add"
    $ \ctx p -> do
      case ctx ^. #guild of
        Just g -> do
          void $ usingConn (execute $ addPrefix (getID @Guild g, p))
          void $ tell @L.Text ctx ("Added prefix: " <> p)
        Nothing ->
          void $ tell @L.Text ctx "Can't use this unless you're in a guild" -- TODO check
