-- |
module CalamityBot.Utils.Process
  ( runCmdLazy,
  )
where

import qualified Data.ByteString.Lazy as LB
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)
import System.Exit

runCmdLazy :: FilePath -> [String] -> IO (Either LB.ByteString LB.ByteString)
runCmdLazy exec args = do
  (exitCode, out, err) <- readCreateProcessWithExitCode (proc exec args) ""
  pure $ case exitCode of
    ExitSuccess ->
      Right out
    ExitFailure {} ->
      Left err
