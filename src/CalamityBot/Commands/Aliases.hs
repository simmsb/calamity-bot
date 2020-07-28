-- | Alias commands
module CalamityBot.Commands.Aliases
  ( aliasGroup,
  )
where

import Calamity
import Calamity.Commands
import CalamityBot.Db
import CalamityBot.Pagination
import Data.Default.Class
import Control.Lens hiding (Context)
import qualified Data.Text.Lazy as L
import Database.Beam (runDelete, runInsert, runSelectReturningList, runSelectReturningOne)
import qualified Polysemy as P
import Polysemy.Immortal
import Polysemy.Timeout

aliasGroup :: (BotC r, P.Members '[DBEff, Immortal, Timeout, ParsePrefix] r) => P.Sem (DSLState r) ()
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

    help (const "List your aliases") $
      command @'[] "list" \ctx ->
        let get Initial = usingConn (runSelectReturningList $ aliasesForPaginatedInitial (getID user, width))
            get (MoveLeft f) = reverse <$> usingConn (runSelectReturningList $ aliasesForPaginatedBefore (getID user, width, f ^. #aliasName))
            get (MoveRight l) = usingConn (runSelectReturningList $ aliasesForPaginatedAfter (getID user, width, l ^. #aliasName))
            render aliases =
              def & #title ?~ ("Aliases for: " <> displayUser user)
                & #description ?~ renderDesc aliases
            renderDesc :: [DBAlias] -> LText
            renderDesc aliases = formatPagination2 ["name", "alias"] aliases (\r -> (r ^. #aliasName, r ^. #aliasValue))
            width = 10
            user = ctx ^. #user
         in paginate get (renderPaginationEmbed render) ctx

    help (const "Remove an alias") $
      command @'[Named "name" L.Text] "remove" \ctx name -> do
        let user = ctx ^. #user
        usingConn . runDelete $ removeAliasByName (getID user, name)
        void $ tell @L.Text ctx "Removed that alias if it existed"
