-- |
module CalamityBot.Utils.Process
  ( runCmdLazy,
  )
where

import Control.Exception (evaluate, finally)
import qualified Data.ByteString as B
import System.IO (hClose, hGetContents, hIsEOF, hSetBinaryMode)
import System.Process (runInteractiveProcess, terminateProcess, waitForProcess)
import System.Exit

runCmdLazy :: FilePath -> [String] -> (IO (Either String B.ByteString) -> IO a) -> IO a
runCmdLazy exec args handler = do
  (inp, out, err, pid) <- runInteractiveProcess exec args Nothing Nothing
  hSetBinaryMode out True
  hClose inp
  let fetch = do
        eof <- hIsEOF out
        if eof
          then do
            err' <- hGetContents err
            _ <- evaluate (length err')
            ret <- waitForProcess pid
            case ret of
              ExitSuccess -> return (Left "")
              ExitFailure {} -> return (Left err')
          else do
            Right <$> B.hGetContents out
  handler fetch `finally` do
    terminateProcess pid
    _ <- waitForProcess pid
    return ()
