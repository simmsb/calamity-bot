-- |  Reminder related commands
module CalamityBot.Commands.Reminders
    ( reminderGroup,
     ) where

import Calamity.Commands
import Calamity
import CalamityBot.Db
import CalamityBot.Pagination
import CalamityBot.Utils
import Control.Lens hiding (Context)
import qualified Data.Text.Lazy as L
import qualified Polysemy as P
import TextShow (TextShow (showtl))
import Replace.Megaparsec
import Data.Dates.Parsing
import Time.System
import Data.Hourglass
import Data.Traversable
import Polysemy.Immortal
import Control.Concurrent (threadDelay)
import Database.Beam (runDelete, runInsert, runSelectReturningList)
import qualified Polysemy.Async as P

mergePreSuff :: L.Text -> L.Text -> L.Text
mergePreSuff s p = L.strip (L.strip s <> " " <> L.strip p)

timeTable :: [(L.Text, Seconds)]
timeTable = [
  ("year", 365 * day),
  ("week", 7 * day),
  ("day", day),
  ("hour", toSeconds $ Hours 1),
  ("minute", toSeconds $ Minutes 1),
  ("second", 1)]
  where day = toSeconds $ Hours 24

humanListConcat :: [L.Text] -> L.Text
humanListConcat xs = case nonEmpty xs of
  Nothing -> ""
  Just (x :| []) -> x
  Just x -> L.intercalate ", " (init x) <> ", and " <> last x

formatTimeDiff :: DateTime -- ^ Start
               -> DateTime -- ^ End
               -> L.Text
formatTimeDiff start end =
  let diff = timeDiff end start
  in filter ((/= 0) . snd) (go diff)
     & map (\(name, Seconds n) -> showtl n <> " " <> name <> memptyIfTrue (n == 1) "s")
     & humanListConcat
  where
    go :: Seconds -> [(L.Text, Seconds)]
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

fmtReminderMessage :: DBReminder -> L.Text
fmtReminderMessage r = mention (r ^. #reminderUserId) <> ", " <> delta <> " ago, you asked me to remind you about: " <> (r ^. #reminderMessage)
  where delta = formatTimeDiff (utcTimeToHourglass (r ^. #reminderCreated)) (utcTimeToHourglass (r ^. #reminderTarget))

reminderTask :: (BotC r, P.Member DBEff r) => P.Sem r ()
reminderTask = forever do
  upcoming <- usingConn $ runSelectReturningList upcomingReminders
  void $ P.sequenceConcurrently $ (P.embed (threadDelaySeconds 60) : map processReminder upcoming)

  where processReminder :: (BotC r, P.Member DBEff r) => DBReminder -> P.Sem r ()
        processReminder r = do
          P.embed . sleepUntil $ utcTimeToHourglass (r ^. #reminderTarget)
          let msg = fmtReminderMessage r
          resp <- tell (r ^. #reminderChannelId) msg
          case resp of
            Left _ ->
              void $ tell (r ^. #reminderUserId) msg
            _ -> pure ()
          usingConn (runDelete $ removeReminder r)


reminderGroup :: (BotC r, P.Members '[DBEff, Immortal] r) => P.Sem (DSLState r) ()
reminderGroup = void
  . help (const "Commands related to making reminders")
  . groupA "remind" ["reminder", "reminders"]
  $ do
    void $ createImmortal (const reminderTask)

    help (const "Add a reminder") $
      command @'[KleenePlusConcat L.Text] "add" \ctx msg -> do
        now <- P.embed dateCurrent
        let cfg = defaultConfig now
        case breakCap (pDateTime @_ @L.Text cfg) msg of
          Just (prefix, when_, suffix) -> do
            let msg' = mergePreSuff prefix suffix
            let user = ctx ^. #user
            let chan = ctx ^. #channel
            let now' = hourglassToUTCTime now
            let when' = hourglassToUTCTime when_
            if when_ < now
              then void $ tell @L.Text ctx "That time is in the past!"
              else do
              let deltaMsg = formatTimeDiff now when_
              void $ usingConn (runInsert (addReminder (getID user, getID chan, msg', now', when')))
              void $ tell @L.Text ctx ("Ok, I'll remind you about: " <> codeline msg' <> ", in: " <> deltaMsg)
          Nothing ->
            void $ tell @L.Text ctx "I couldn't parse the times from that"

    help (const "List your reminders") $
      command @'[Named "page" (Maybe Natural)] "list" \ctx (pred . fromIntegral . fromMaybe 1 -> page) -> do
        let width = 10
        let user = ctx ^. #user
        reminders <- usingConn (runSelectReturningList $ allRemindersForPaginated (getID user, width, page))
        let formatted = formatPagination page width reminders (^. #reminderMessage)
        void $ tell ctx formatted

    help (const "Remove a reminder") $
      command @'[Named "index" Natural] "remove" \ctx (fromIntegral -> idx) -> do
        let user = ctx ^. #user
        usingConn (runDelete $ removeReminderByIdx (getID user, idx))
        void $ tell @L.Text ctx "Removed that reminder if it existed"
