-- |  Reminder related commands
module CalamityBot.Commands.Reminders (
  reminderGroup,
) where

import Calamity
import Calamity.Commands
import Calamity.Commands.Context (FullContext)
import Calamity.Internal.Utils
import CalamityBot.Db
import CalamityBot.Utils.Pagination
import CalamityBot.Utils.Utils
import Control.Concurrent (threadDelay)
import Control.Lens hiding (Context)
import Data.Default.Class
import Data.Hourglass
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (utc, utcToZonedTime, zonedTimeToUTC)
import Data.Traversable
import Database.Beam (runDelete, runInsert, runSelectReturningList)
-- import qualified Duckling.Core as D
-- import qualified Duckling.Time.Types as D
import qualified Polysemy as P
import qualified Polysemy.Async as P
import Polysemy.Immortal
import Polysemy.Timeout
import TextShow (TextShow (showt))
import Time.System

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
  Just x -> T.intercalate ", " (init x) <> ", and " <> last x

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
  void $ P.sequenceConcurrently $ (P.embed (threadDelaySeconds 6) : map processReminder upcoming)
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

reminderGroup :: (BotC r, P.Members '[DBEff, Immortal, Timeout] r) => P.Sem (DSLState FullContext r) ()
reminderGroup = void
  . help (const "Commands related to making reminders")
  . groupA "remind" ["reminder", "reminders"]
  $ do
    void $ createImmortal (const reminderTask)

    -- help (const "Add a reminder") $
    --   command @'[KleenePlusConcat Text] "add" \ctx msg -> do
    --     now <- P.embed getCurrentTime
    --     let locale = D.makeLocale D.EN Nothing
    --         dnow = D.fromZonedTime $ utcToZonedTime utc now
    --         dctx = D.Context dnow locale
    --         opts = D.Options True
    --         parsedEntities = D.parse msg dctx opts [D.Seal D.Time]

    --     case viaNonEmpty head parsedEntities of
    --       Just e -> do
    --         let msg' = toLazy $ niceRemoveRange (e ^. #start) (e ^. #end) msg
    --             user = ctx ^. #user
    --             chan = ctx ^. #channel
    --         when <- case e ^. #value of
    --           D.RVal D.Time (D.TimeValue (D.SimpleValue (D.InstantValue when _)) _ _) ->
    --             pure $ zonedTimeToUTC when
    --           _ ->
    --             fail "Invalid time"
    --         if when < now
    --           then fail "That time is in the past"
    --           else do
    --             let deltaMsg = formatTimeDiff (utcTimeToHourglass now) (utcTimeToHourglass when)
    --             void $ usingConn (runInsert (addReminder (getID user, getID chan, msg', now, when)))
    --             void $ tell @T.Text ctx ("Ok, I'll remind you about: " <> codeline msg' <> ", in: " <> deltaMsg)
    --       Nothing ->
    --         fail "I couldn't parse the times from that"

    help (const "List your reminders") $
      command @'[] "list" \ctx ->
        let get Initial = usingConn (runSelectReturningList $ remindersForPaginatedInitial (getID user, width))
            get (MoveLeft f) = reverse <$> usingConn (runSelectReturningList $ remindersForPaginatedBefore (getID user, width, f ^. #reminderTarget, f ^. #reminderId))
            get (MoveRight l) = usingConn (runSelectReturningList $ remindersForPaginatedAfter (getID user, width, l ^. #reminderTarget, l ^. #reminderId))
            render reminders =
              def & #title ?~ ("Reminders for: " <> displayUser user)
                & #description ?~ renderDesc reminders
            renderDesc :: [DBReminder] -> Text
            renderDesc reminders = formatPagination2 ["id", "message"] reminders (\r -> (r ^. #reminderId, r ^. #reminderMessage))
            width = 10
            user = ctx ^. #user
         in paginate get (renderPaginationEmbed render) ctx

    help (const "Remove a reminder") $
      command @'[Named "reminder_id" Text] "remove" \ctx rid -> do
        let user = ctx ^. #user
        usingConn (runDelete $ removeReminder (getID user, rid))
        void $ tell @T.Text ctx "Removed that reminder if it existed"
