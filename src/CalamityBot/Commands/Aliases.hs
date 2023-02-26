-- | Alias commands
module CalamityBot.Commands.Aliases (
  aliasGroup,
) where

import Calamity
import Calamity.Commands
import Calamity.Commands.Context (FullContext)
import CalamityBot.Db.Aliases (
  addAlias,
  aliasesForPaginatedAfter,
  aliasesForPaginatedBefore,
  aliasesForPaginatedInitial,
  getAlias,
  removeAliasByName,
 )
import CalamityBot.Db.Eff (DBEff, usingConn)
import CalamityBot.Db.Schema (DBAlias)
import CalamityBot.Utils.Pagination
import CalamityCommands.Context (ConstructContext)
import CalamityCommands.ParsePrefix (ParsePrefix)
import CalamityCommands.Utils (handleCommands)
import Data.Default.Class
import Data.Text qualified as T
import Database.Beam (runDelete, runInsert, runSelectReturningList, runSelectReturningOne)
import Optics
import Polysemy qualified as P
import Polysemy.Immortal
import Polysemy.Timeout
import Control.Monad (void)

aliasGroup :: (BotC r, P.Members '[DBEff, Immortal, Timeout, ParsePrefix Message, ConstructContext (Message, User, Maybe Member) FullContext IO ()] r) => P.Sem (DSLState FullContext r) ()
aliasGroup = void
  . help (const "Commands related to making aliases")
  . groupA "alias" ["aliases"]
  $ do
    handler <- fetchHandler
    react @('CustomEvt CommandNotFound) \(CommandNotFound msg usr mem path) ->
      case path of
        (aliasName : _) -> do
          alias' <- usingConn (runSelectReturningOne $ getAlias (getID @User msg, aliasName))
          case alias' of
            Just alias -> do
              void $ handleCommands handler (msg, usr, mem) "" (alias ^. #aliasValue)
            Nothing -> pure ()
        [] -> pure ()

    help (const "Add an alias") $
      command @'[Named "name" T.Text, Named "command" (KleenePlusConcat T.Text)] "add" \ctx name cmd -> do
        let user = ctx ^. #user
        void . usingConn . runInsert $ addAlias (getID user, name, cmd)
        void $ tell @T.Text ctx ("Added the alias: " <> codeline name <> ", with the value: " <> codeline cmd)

    help (const "List your aliases") $
      command @'[] "list" \ctx ->
        let get Initial = usingConn (runSelectReturningList $ aliasesForPaginatedInitial (getID user, width))
            get (MoveLeft f) = reverse <$> usingConn (runSelectReturningList $ aliasesForPaginatedBefore (getID user, width, f ^. #aliasName))
            get (MoveRight l) = usingConn (runSelectReturningList $ aliasesForPaginatedAfter (getID user, width, l ^. #aliasName))
            render aliases =
              def
                & #title ?~ ("Aliases for: " <> displayUser user)
                & #description ?~ renderDesc aliases
            renderDesc :: [DBAlias] -> T.Text
            renderDesc aliases = formatPagination2 ["name", "alias"] aliases (\r -> (r ^. #aliasName, r ^. #aliasValue))
            width = 10
            user = ctx ^. #user
         in paginate get (renderPaginationEmbed render) ctx

    help (const "Remove an alias") $
      command @'[Named "name" T.Text] "remove" \ctx name -> do
        let user = ctx ^. #user
        usingConn . runDelete $ removeAliasByName (getID user, name)
        void $ tell @T.Text ctx "Removed that alias if it existed"
