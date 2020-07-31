-- | utilities
module CalamityBot.Utils.Utils
  ( utcTimeToHourglass,
    hourglassToUTCTime,
  )
where

import qualified Data.Hourglass as H
import qualified Data.Time as T
import qualified Data.Time.Clock.POSIX as T
import qualified Time.Compat as H

utcTimeToHourglass :: T.UTCTime -> H.DateTime
utcTimeToHourglass u =
  let date = T.utctDay u & T.toModifiedJulianDay & H.dateFromTAIEpoch
      time = T.utctDayTime u & H.diffTimeToTimeOfDay
   in H.DateTime date time

hourglassToUTCTime :: H.DateTime -> T.UTCTime
hourglassToUTCTime u =
  let sincePosix = H.timeGetElapsedP u & toRational & fromRational & T.secondsToNominalDiffTime
   in T.posixSecondsToUTCTime sincePosix
