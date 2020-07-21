-- | Alias commands
module CalamityBot.Commands.Aliases
  ( aliasGroup,
  )
where

import Calamity
import Calamity.Commands
import CalamityBot.Db
import CalamityBot.Pagination
import Control.Lens hiding (Context)
import qualified Data.Text.Lazy as L
import Database.Beam (runDelete, runInsert, runSelectReturningList, runSelectReturningOne)
import qualified Polysemy as P
import Polysemy.Immortal

aliasGroup :: (BotC r, P.Members '[DBEff, Immortal, ParsePrefix] r) => P.Sem (DSLState r) ()
aliasGroup = void
  . help (const "Commands related to making aliases")
  . groupA "alias" ["aliases"]
  $ do
    handler <- fetchHandler
    react @('CustomEvt "command-not-found" (Message, [L.Text])) \(msg, path) ->
      case path of
        (aliasName : _) -> do
          alias' <- usingConn (runSelectReturningOne $ getAlias (getID @User msg, aliasName))
          case alias' of
            Just alias -> do
              void $ handleCommands handler msg "" (alias ^. #aliasValue)
            Nothing -> pure ()
        [] -> pure ()

    help (const "Add an alias") $
      command @'[Named "name" L.Text, Named "command" (KleenePlusConcat L.Text)] "add" \ctx name cmd -> do
        let user = ctx ^. #user
        void . usingConn . runInsert $ addAlias (getID user, name, cmd)
        void $ tell @L.Text ctx ("Added the alias: " <> codeline name <> ", with the value: " <> codeline cmd)

    -- help (const "List your aliases") $
    --   command @'[Named "page" (Maybe Natural)] "list" \ctx (pred . fromIntegral . fromMaybe 1 -> page) -> do
    --     let width = 10
    --     let user = ctx ^. #user
    --     aliases <- usingConn (runSelectReturningList $ allAliasesForPaginated (getID user, width, page))
    --     let formatted = formatPagination2 page width aliases (\a -> (a ^. #aliasName, a ^. #aliasValue))
    --     void $ tell ctx formatted

    help (const "Remove an alias") $
      command @'[Named "name" L.Text] "remove" \ctx name -> do
        let user = ctx ^. #user
        usingConn . runDelete $ removeAliasByName (getID user, name)
        void $ tell @L.Text ctx "Removed that alias if it existed"
