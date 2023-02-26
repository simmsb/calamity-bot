{-# LANGUAGE QuasiQuotes #-}

-- |  Reminder related commands
module CalamityBot.Commands.Reminders (
  reminderGroup,
) where

import Calamity
import Calamity.Commands
import Calamity.Commands.Context (FullContext)
import Calamity.Internal.Utils
import CalamityBot.Db.Eff (DBEff, usingConn)
import CalamityBot.Db.Reminders (
  addReminder,
  remindersForPaginatedAfter,
  remindersForPaginatedBefore,
  remindersForPaginatedInitial,
  removeReminder,
  upcomingReminders,
 )
import CalamityBot.Db.Schema (DBReminder)
import CalamityBot.Utils.Pagination
import CalamityBot.Utils.Utils
import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Default.Class
import Data.Hourglass
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Traversable
import Database.Beam (runDelete, runInsert, runSelectReturningList)
import Network.HTTP.Req qualified as Req
import Optics
import Polysemy qualified as P
import Polysemy.Async qualified as P
import Polysemy.Immortal
import Polysemy.Timeout
import TextShow (TextShow (showt))
import Time.System
import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NE
import Universum (evalState, MonadState (..))
import Control.Monad (void)

mergePreSuff :: T.Text -> T.Text -> T.Text
mergePreSuff s p = T.strip (T.strip s <> " " <> T.strip p)

niceRemoveRange :: Int -> Int -> T.Text -> T.Text
niceRemoveRange s e t = mergePreSuff (T.take s t) (T.drop e t)

timeTable :: [(T.Text, Seconds)]
timeTable =
  [ ("year", 365 * day)
  , ("week", 7 * day)
  , ("day", day)
  , ("hour", toSeconds $ Hours 1)
  , ("minute", toSeconds $ Minutes 1)
  , ("second", 1)
  ]
  where
    day = toSeconds $ Hours 24

humanListConcat :: [T.Text] -> T.Text
humanListConcat xs = case nonEmpty xs of
  Nothing -> ""
  Just (x :| []) -> x
  Just x -> T.intercalate ", " (NE.init x) <> ", and " <> NE.last x

memptyIfTrue :: Monoid m => Bool -> m -> m
memptyIfTrue p val = if p then mempty else val

formatTimeDiff ::
  -- | Start
  DateTime ->
  -- | End
  DateTime ->
  T.Text
formatTimeDiff start end =
  let diff = timeDiff end start
   in filter ((/= 0) . snd) (go diff)
        & map (\(name, Seconds n) -> showt n <> " " <> name <> memptyIfTrue (n == 1) "s")
        & humanListConcat
  where
    go :: Seconds -> [(T.Text, Seconds)]
    go duration = flip evalState duration $ for timeTable \(name, period) -> do
      duration' <- get
      let (n, duration'') = divMod duration' period
      put duration''
      pure (name, n)

threadDelaySeconds :: Int -> IO ()
threadDelaySeconds = threadDelay . (* 1000000)

sleepUntil :: DateTime -> IO ()
sleepUntil when_ = do
  now <- dateCurrent
  let diff = fromIntegral $ timeDiff when_ now
  threadDelaySeconds diff

fmtReminderMessage :: DBReminder -> T.Text
fmtReminderMessage r = mention (r ^. #reminderUserId) <> ", " <> delta <> " ago, you asked me to remind you about: " <> (r ^. #reminderMessage)
  where
    delta = formatTimeDiff (utcTimeToHourglass (r ^. #reminderCreated)) (utcTimeToHourglass (r ^. #reminderTarget))

reminderTask :: (BotC r, P.Member DBEff r) => P.Sem r ()
reminderTask = untilJustFinalIO do
  upcoming <- usingConn $ runSelectReturningList upcomingReminders
  void $ P.sequenceConcurrently (P.embed (threadDelaySeconds 6) : map processReminder upcoming)
  pure Nothing
  where
    processReminder :: (BotC r, P.Member DBEff r) => DBReminder -> P.Sem r ()
    processReminder r = do
      P.embed . sleepUntil $ utcTimeToHourglass (r ^. #reminderTarget)
      let msg = fmtReminderMessage r
      resp <- tell (r ^. #reminderChannelId) msg
      case resp of
        Left _ ->
          void $ tell (r ^. #reminderUserId) msg
        _ -> pure ()
      usingConn (runDelete $ removeReminder (r ^. #reminderUserId, r ^. #reminderId))

data DucklingTime = DucklingTime
  { start :: Int
  , end :: Int
  , time :: ZonedTime
  }
  deriving (Generic)

data DucklingResponseValue = DucklingResponseValue
  { value :: ZonedTime
  , grain :: T.Text
  }
  deriving (Generic, FromJSON)

data DucklingResponseValues = DucklingResponseValues
  { values :: [DucklingResponseValue]
  , value :: ZonedTime
  , grain :: T.Text
  }
  deriving (Generic, FromJSON)

data DucklingResponse = DucklingResponse
  { body :: T.Text
  , start :: Int
  , value :: DucklingResponseValues
  , end :: Int
  , dim :: T.Text
  , latent :: Bool
  }
  deriving (Generic, FromJSON)

parseWithDuckling :: T.Text -> IO (Maybe DucklingTime)
parseWithDuckling input = do
  let (url, options) = [Req.urlQ|http://duckling:8000/parse|]
      params =
        mconcat
          [ "locale" Req.=: ("en_GB" :: T.Text)
          , "text" Req.=: input
          , "dims" Req.=: ("[\"time\"]" :: T.Text)
          ]
  r <- Req.runReq Req.defaultHttpConfig $ Req.req Req.POST url (Req.ReqBodyUrlEnc params) Req.jsonResponse options
  let val :: Maybe (NonEmpty DucklingResponse) = Req.responseBody r
  case val of
    Just (val' :| _) ->
      pure . Just $
        DucklingTime
          { start = val' ^. #start
          , end = val' ^. #end
          , time = val' ^. #value % #value
          }
    _ -> pure Nothing

reminderGroup :: (BotC r, P.Members '[DBEff, Immortal, Timeout] r) => P.Sem (DSLState FullContext r) ()
reminderGroup = void
  . help (const "Commands related to making reminders")
  . groupA "remind" ["reminder", "reminders"]
  $ do
    void $ createImmortal (const reminderTask)

    help (const "Add a reminder") $
      command @'[KleenePlusConcat T.Text] "add" \ctx msg -> do
        parsed <- P.embed $ parseWithDuckling msg
        now <- P.embed getCurrentTime

        case parsed of
          Just e -> do
            let msg' = niceRemoveRange (e ^. #start) (e ^. #end) msg
                user = ctx ^. #user
                chan = ctx ^. #channel
                whenUtc = zonedTimeToUTC (e ^. #time)
            if whenUtc < now
              then fail "That time is in the past"
              else do
                let deltaMsg = formatTimeDiff (utcTimeToHourglass now) (utcTimeToHourglass whenUtc)
                void $ usingConn (runInsert (addReminder (getID user, getID chan, msg', now, whenUtc)))
                void $ tell @T.Text ctx ("Ok, I'll remind you about: " <> codeline msg' <> ", in: " <> deltaMsg)
          Nothing ->
            fail "I couldn't parse the times from that"

    help (const "List your reminders") $
      command @'[] "list" \ctx ->
        let get Initial = usingConn (runSelectReturningList $ remindersForPaginatedInitial (getID user, width))
            get (MoveLeft f) = reverse <$> usingConn (runSelectReturningList $ remindersForPaginatedBefore (getID user, width, f ^. #reminderTarget, f ^. #reminderId))
            get (MoveRight l) = usingConn (runSelectReturningList $ remindersForPaginatedAfter (getID user, width, l ^. #reminderTarget, l ^. #reminderId))
            render reminders =
              def
                & #title ?~ ("Reminders for: " <> displayUser user)
                & #description ?~ renderDesc reminders
            renderDesc :: [DBReminder] -> T.Text
            renderDesc reminders = formatPagination2 ["id", "message"] reminders (\r -> (r ^. #reminderId, r ^. #reminderMessage))
            width = 10
            user = ctx ^. #user
         in paginate get (renderPaginationEmbed render) ctx

    help (const "Remove a reminder") $
      command @'[Named "reminder_id" T.Text] "remove" \ctx rid -> do
        let user = ctx ^. #user
        usingConn (runDelete $ removeReminder (getID user, rid))
        void $ tell @T.Text ctx "Removed that reminder if it existed"
