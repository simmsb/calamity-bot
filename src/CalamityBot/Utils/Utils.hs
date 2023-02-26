-- | utilities
module CalamityBot.Utils.Utils (
  utcTimeToHourglass,
  hourglassToUTCTime,
  requireExecutable,
  withTempDir,
  withTempFile,
) where

import Data.Hourglass qualified as H
import Data.Time qualified as T
import Data.Time.Clock.POSIX qualified as T
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Environment (setEnv)
import System.FilePath ((<.>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Time.Compat qualified as H
import Data.Function

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
    Nothing -> error $ "Couldn't find executable: " <> exec
    Just path -> return path

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name action = do
  setEnv "TMPDIR" "/tmp"
  createDirectoryIfMissing False "/tmp"
  withSystemTempDirectory name action

withTempFile :: String -> String -> (FilePath -> IO a) -> IO a
withTempFile name ext action = do
  setEnv "TMPDIR" "/tmp"
  createDirectoryIfMissing False "/tmp"
  withSystemTempFile (name <.> ext) $ \path hd ->
    hClose hd >> action path
