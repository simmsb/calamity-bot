module CalamityBot.Utils.Process (
  runCmdLazy,
) where

import Data.ByteString.Lazy qualified as LB
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process qualified as CreateProcess
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)

runCmdLazy :: FilePath -> [String] -> IO (Either LB.ByteString LB.ByteString)
runCmdLazy exec args = do
  (exitCode, out, err) <- readCreateProcessWithExitCode (CreateProcess.proc exec args) ""
  pure $ case exitCode of
    ExitSuccess ->
      Right out
    ExitFailure {} ->
      Left err
