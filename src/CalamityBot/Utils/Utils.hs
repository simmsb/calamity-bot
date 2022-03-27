-- | utilities
module CalamityBot.Utils.Utils (
  utcTimeToHourglass,
  hourglassToUTCTime,
  requireExecutable,
  withTempDir,
  withTempFile,
) where

import qualified Data.Hourglass as H
import qualified Data.Time as T
import qualified Data.Time.Clock.POSIX as T
import System.Directory (findExecutable)
import System.FilePath ((<.>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
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

requireExecutable :: String -> IO FilePath
requireExecutable exec = do
  mbPath <- findExecutable exec
  case mbPath of
    Nothing -> error $ "Couldn't find executable: " <> toText exec
    Just path -> return path

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory

withTempFile :: String -> String -> (FilePath -> IO a) -> IO a
withTempFile name ext action =
  withSystemTempFile (name <.> ext) $ \path hd ->
    hClose hd >> action path
