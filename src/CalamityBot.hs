module CalamityBot (
  runBot,
) where

import Calamity
import Calamity.Cache.Eff (CacheEff, getGuilds, getMessage)
import Calamity.Cache.InMemory
import Calamity.Commands as C
import Calamity.Commands.Context (FullContext, useFullContext)
import Calamity.Gateway.Types (StatusUpdateData (..))
import Calamity.HTTP as H
import Calamity.Metrics.Noop
import Calamity.Types.Model.Channel.Component (ButtonStyle (ButtonPrimary), Component (ActionRow', Button'), CustomID (..), button)
import qualified Calamity.Types.Model.Presence.Activity
import CalamityBot.Commands.Aliases (aliasGroup)
import CalamityBot.Commands.Crap (crapGroup)
import CalamityBot.Commands.Prefix (prefixGroup)
-- import CalamityBot.Commands.Reanimate (reanimateGroup)
import CalamityBot.Commands.Reminders (reminderGroup)
import CalamityBot.Db.Eff (runDBEffPooled)
import CalamityBot.PrefixHandler
import CalamityBot.Utils.Config
import qualified Data.ByteString.Char8 as BS
import Data.Pool (createPool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import qualified Df1
import qualified Di
import qualified Di.Core as DiC
import DiPolysemy
import Optics
import Polysemy
import qualified Polysemy.AtomicState as P
import Polysemy.Immortal
import Polysemy.Prometheus (runMetricsPrometheusIO)
import Polysemy.Timeout
import System.Environment
import Text.Pretty.Simple
import TextShow

cfg :: HashMap Text Text
cfg =
  fromList
    [ ("stickbug_path", "assets/stickbug.mp4")
    , ("bunny_path", "assets/bunny.mp4")
    , ("goblin_path", "assets/goblin.mp4")
    ]

-- filterDi :: DiC.Di l Di.Path m -> DiC.Di l Di.Path m
-- filterDi = DiC.filter (\_ p _ -> Df1.Push "calamity" `notElem` p)

c :: BotC r => Sem (P.AtomicState Int ': DSLState FullContext r) ()
c = void $
  raise $
    command @'[] "hello" $
      \ctx -> void do
        pure ()

-- x <- P.atomicGet @Int
-- tell ctx (show x)

example :: BotC r => Sem (DSLState FullContext r) ()
example = void do
  -- putStrLn "doing some IO..."
  tvar <- embed $ newTVarIO 7
  -- P.runAtomicStateTVar tvar c
  pure ()

runBot :: IO ()
runBot = Di.new \di -> do
  token <- T.pack <$> getEnv "BOT_TOKEN"
  db_path <- BS.pack <$> getEnv "DB_STRING"
  pool <- createPool (connectPostgreSQL db_path) close 2 0.5 2
  void . runFinal
    . embedToFinal
    . configAsConst cfg
    . timeoutToIOFinal
    . immortalToIOFinal
    . runDBEffPooled pool
    . runCacheInMemory
    . runMetricsPrometheusIO
    . useDatabasePrefix "c!"
    . useFullContext
    . runDiToIO di
    -- . DiPolysemy.local filterDi
    . runBotIO (BotToken token) defaultIntents
    $ do
      DiPolysemy.push "calamity-bot" $ addCommands do
        void helpCommand
        prefixGroup
        reminderGroup
        aliasGroup
        -- reanimateGroup
        crapGroup
        command @'[] "testup" \ctx -> do
          info @Text "hi"
          case ctx ^. #guild of
            Just g -> do
              member <- upgrade @Calamity.Member (getID @Guild g, getID @Calamity.Member $ ctx ^. #user)
              print member
              member' <- invoke $ H.GetGuildMember g (ctx ^. #user)
              print member'
            _ -> putStrLn "not a guild"
        hide do
          C.group "cantseethis" do
            command @'[] "nope" \ctx ->
              void $ tell @T.Text ctx "You found me"
          command @'[] "testguild" \ctx ->
            void $ print $ ctx ^. #guild
          command @'[Calamity.Member] "testmember" \ctx member ->
            void $ tell @T.Text ctx (showt member)
          command @'[] "cantseeme" \ctx ->
            void $ tell @T.Text ctx "You found me"
          command @'[] "prevmsg" \ctx -> do
            Right msgs <- invoke $ GetChannelMessages (ctx ^. #channel) (Just . ChannelMessagesBefore $ ctx ^. #message % #id) (Just $ ChannelMessagesLimit 10)
            info . showt $ msgs
          command @'[Snowflake Message] "inspectmsg" \ctx mid -> do
            Just msg <- getMessage mid
            void . tell ctx . codeblock' Nothing . fromLazy $ pShowNoColor msg
          command @'[Snowflake Message] "inspectmsgF" \ctx mid -> do
            Right msg <- invoke $ GetMessage (ctx ^. #channel) mid
            void . tell ctx . codeblock' Nothing . fromLazy $ pShowNoColor msg
          command @'[] "treply" \ctx ->
            void $ reply @Text (ctx ^. #message) "hello"
          command @'[] "components" \ctx -> do
            void . tell ctx $
              [ button ButtonPrimary (CustomID "test")
                  & #label ?~ "test"
              , button ButtonDanger (CustomID "test2")
                  & #label ?~ "test2"
              ]
      -- command @'[] "listguilds" \ctx -> do
      --   guilds <- getGuilds
      --   let gf = T.unlines ["id: " <> showtl (g ^. #id) <> ", name: " <> (g ^. #name) | g <- guilds]
      --   void . tell ctx . codeblock' Nothing $ gf
      -- command @'[] "spam" \ctx ->
      --   replicateM_ 10 . P.async $ reply @Text (ctx ^. #message) "hello"
      -- command @'[] "makeChannel" \ctx -> do
      --   case ctx ^. #guild of
      --     Just g -> do
      --       Right ch <-
      --         invoke $
      --           CreateGuildChannel g $
      --             ChannelCreateData
      --               { name = "test"
      --               , type_ = Just GuildTextType
      --               , topic = Nothing
      --               , bitrate = Nothing
      --               , userLimit = Nothing
      --               , rateLimitPerUser = Nothing
      --               , position = Nothing
      --               , permissionOverwrites = Nothing
      --               , parentID = Nothing
      --               , nsfw = Nothing
      --               }
      --       void . reply @Text (ctx ^. #message) $ showt ch
      --     Nothing ->
      --       void . reply @Text (ctx ^. #message) $ "not a guild lol"
      react @( 'CustomEvt (CtxCommandError FullContext)) \(CtxCommandError ctx e) -> do
        info $ "Command failed with reason: " <> showt e
        case e of
          ParseError n r ->
            void . tell ctx $
              "Failed to parse parameter: " <> codeline n
                <> ", with reason: "
                <> codeblock' Nothing r
          CheckError n r ->
            void . tell ctx $
              "The following check failed: " <> codeline n
                <> ", with reason: "
                <> codeblock' Nothing r
          InvokeError n r ->
            void . tell ctx $
              "The command: " <> codeline n
                <> ", failed with reason: "
                <> codeblock' Nothing r
      react @'MessageCreateEvt \m -> do
        info . showt $ m
      react @'ReadyEvt \_ -> do
        sendPresence
          StatusUpdateData
            { since = Nothing
            , activities = [Calamity.Types.Model.Presence.Activity.activity "Prefix: c!" Game]
            , status = Online
            , afk = False
            }
      pure ()
