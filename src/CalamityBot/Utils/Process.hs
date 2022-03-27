-- |
module CalamityBot.Utils.Process (
  runCmdLazy,
) where

import qualified Data.ByteString.Lazy as LB
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

runCmdLazy :: FilePath -> [String] -> IO (Either LB.ByteString LB.ByteString)
runCmdLazy exec args = do
  (exitCode, out, err) <- readCreateProcessWithExitCode (proc exec args) ""
  pure $ case exitCode of
    ExitSuccess ->
      Right out
    ExitFailure {} ->
      Left err
