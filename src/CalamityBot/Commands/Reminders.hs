-- |  Reminder related commands
module CalamityBot.Commands.Reminders
    ( reminderGroup,
     ) where

import Calamity.Commands
import Calamity
import CalamityBot.Db
import CalamityBot.Utils
import Control.Lens hiding (Context)
import qualified Data.Text.Lazy as L
import qualified Polysemy as P
import Squeal.PostgreSQL (MonadPQ (execute))
import TextShow (TextShow (showtl))
import Replace.Megaparsec
import Data.Dates.Parsing
import Time.System
import Data.Hourglass
import Data.Traversable

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
     & map (\(name, Seconds n) -> showtl n <> " " <> name <> memptyIfTrue (n == 0) "s")
     & humanListConcat
  where
    go :: Seconds -> [(L.Text, Seconds)]
    go duration = flip evalState duration $ for timeTable \(name, period) -> do
      duration' <- get
      let (n, duration'') = divMod duration' period
      put duration''
      pure (name, n)

-- TODO: Delta times
-- TODO: improve fuzzy date

reminderGroup :: (BotC r, P.Member (DBEff DB) r) => P.Sem (DSLState r) ()
reminderGroup = void
  . help (const "Commands related to making reminders")
  . groupA "remind" ["reminder", "reminders"]
  $ do
    help (const "Add a reminder") $
      command @'[KleenePlusConcat L.Text] "add" \ctx msg -> do
        now <- P.embed dateCurrent
        let cfg = defaultConfig now
        case breakCap (pDateTime @_ @L.Text cfg) msg of
          Just (prefix, when_, suffix) -> do
            let msg' = mergePreSuff prefix suffix
            let user = ctx ^. #user
            let chan = ctx ^. #channel
            let when' = hourglassToUTCTime when_
            if when_ < now
              then void $ tell @L.Text ctx "That time is in the past!"
              else do
              let deltaMsg = formatTimeDiff now when_
              void $ usingConn (execute (addReminder (getID user, getID chan, msg', when')))
              void $ tell @L.Text ctx ("Ok, I'll remind you about: " <> codeline msg' <> ", in: " <> deltaMsg)
          Nothing ->
            void $ tell @L.Text ctx "I couldn't parse the times from that"
