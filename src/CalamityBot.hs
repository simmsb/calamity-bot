{-# LANGUAGE QuasiQuotes #-}

module CalamityBot (
  runBot,
) where

import Calamity
import Calamity.Cache.Eff (getMessage)
import Calamity.Cache.InMemory
import Calamity.Commands as C
import Calamity.Gateway.Types (StatusUpdateData (..))
import Calamity.HTTP as H
import Calamity.Metrics.Noop
import qualified Calamity.Types.Model.Presence.Activity
import CalamityBot.Commands
import CalamityBot.Db
import CalamityBot.PrefixHandler
import CalamityBot.Utils.Config
import qualified Data.ByteString.Char8 as BS
import Data.Pool (createPool)
import qualified Data.Text.Lazy as L
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import qualified Di

-- import qualified Di.Core as DiC

import Control.Lens ((^.))
import DiPolysemy
import Polysemy
-- import qualified Polysemy.Async as P
import Polysemy.Immortal
import Polysemy.Timeout
import System.Environment
import Text.Pretty.Simple
import TextShow

cfg :: HashMap Text Text
cfg =
  fromList
    [ ("stickbug_path", "assets/stickbug.mp4")
    , ("bunny_path", "assets/bunny.mp4")
    ]

-- filterDi :: DiC.Di l p m -> DiC.Di l p m
-- filterDi = DiC.filter (\_ _ _ -> False)

runBot :: IO ()
runBot = Di.new \di -> do
  token <- L.pack <$> getEnv "BOT_TOKEN"
  db_path <- BS.pack <$> getEnv "DB_STRING"
  pool <- createPool (connectPostgreSQL db_path) close 2 0.5 2
  void . runFinal
    . embedToFinal
    . configAsConst cfg
    . timeoutToIOFinal
    . immortalToIOFinal
    . runDBEffPooled pool
    . runCacheInMemory
    . runMetricsNoop
    . useDatabasePrefix "c!"
    . runDiToIO di
    -- . DiPolysemy.local filterDi -- disables calamity logging
    . runBotIO (BotToken token) defaultIntents
    -- . DiPolysemy.local (const di)
    $ do
      DiPolysemy.local (const di) $
        DiPolysemy.push "calamity-bot" $ addCommands do
          void helpCommand
          prefixGroup
          reminderGroup
          aliasGroup
          reanimateGroup
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
                void $ tell @L.Text ctx "You found me"
            command @'[] "cantseeme" \ctx ->
              void $ tell @L.Text ctx "You found me"
            command @'[] "prevmsg" \ctx -> do
              Right msgs <- invoke $ GetChannelMessages (ctx ^. #channel) (Just . ChannelMessagesBefore $ ctx ^. #message . #id) (Just $ ChannelMessagesLimit 10)
              info . showt $ msgs
            command @'[Snowflake Message] "inspectmsg" \ctx mid -> do
              Just msg <- getMessage mid
              void . tell ctx . codeblock' Nothing $ pShowNoColor msg
            command @'[Snowflake Message] "inspectmsgF" \ctx mid -> do
              Right msg <- invoke $ GetMessage (ctx ^. #channel) mid
              void . tell ctx . codeblock' Nothing $ pShowNoColor msg
            command @'[] "treply" \ctx ->
              void $ reply @Text (ctx ^. #message) "hello"
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
      react @( 'CustomEvt "command-error" (Context, CommandError)) \(ctx, e) -> do
        info $ "Command failed with reason: " <> showtl e
        case e of
          ParseError n r ->
            void . tell ctx $
              "Failed to parse parameter: " <> codeline (L.fromStrict n)
                <> ", with reason: "
                <> codeblock' Nothing r
          CheckError n r ->
            void . tell ctx $
              "The following check failed: " <> codeline (L.fromStrict n)
                <> ", with reason: "
                <> codeblock' Nothing r
          InvokeError n r ->
            void . tell ctx $
              "The command: " <> codeline (L.fromStrict n)
                <> ", failed with reason: "
                <> codeblock' Nothing r
      react @ 'ReadyEvt \_ -> do
        sendPresence
          StatusUpdateData
            { since = Nothing
            , game = Just $ Calamity.Types.Model.Presence.Activity.activity "Prefix: c!" Game
            , status = "online"
            , afk = False
            }
      pure ()
