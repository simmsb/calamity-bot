-- | Reanimate fuckery
module CalamityBot.Commands.Reanimate.RenderInMem
    ( renderToMemory
    , renderStickbug
     ) where

import Control.Concurrent (forkIO, getNumCapabilities, newQSemN, signalQSemN, waitQSemN)
import Control.Exception (catch, evaluate, finally, throwIO)
import Control.Concurrent.MVar (isEmptyMVar, modifyMVar_)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Data.Time (diffUTCTime, getCurrentTime)
import Graphics.SvgTree (Number (..))
import Reanimate (Animation, duration)
import Reanimate (frameAt)
import Reanimate.Animation (renderSvg)
import Reanimate.Misc (requireExecutable, withTempDir, withTempFile)
import Numeric
import Reanimate.Parameters (setRootDirectory)
import Reanimate.Render
import System.Exit
import System.FilePath ((</>))
import System.IO (hSetBinaryMode, hGetContents, hIsEOF, hClose)
import System.Process (showCommandForUser, readProcessWithExitCode, runInteractiveProcess, terminateProcess, waitForProcess)
import Text.Printf (printf)
import qualified Data.Text as S

renderStickbug :: (LB.ByteString, Text)
               -> Float
               -> IO (Either String B.ByteString)
renderStickbug (initial, ext) delay = do
  ffmpeg <- requireExecutable "ffmpeg"
  withTempFile (S.unpack ext) $ \initialFile -> do
    writeFileLBS initialFile initial
    let df = showFFloat Nothing delay ""
    runCmdLazy ffmpeg ["-i", initialFile
                      , "-i", "stickbug.mp4" -- TODO: unbad
                      ,"-threads", "0"
                      , "-filter_complex", "[1:v][0:v]scale2ref[v1][v0];"
                                           <> "[v0]trim=end=" <> df <> "[v00];"
                                           <> "[0:a]atrim=end=" <> df <> "[a0];"
                                           <> "[1:a]volume=3[a1]; [v00][a0][v1][a1]concat=n=2:v=1:a=1[v][a]"
                      , "-map", "[v]", "-map", "[a]"
                      , "-c:v", "libx264"
                      , "-r", "30"
                      , "-crf", "18"
                      , "-movflags", "+faststart"
                      , "-pix_fmt", "yuv420p"
                      , "-f", "ismv"
                      , "-"] Prelude.id

renderToMemory :: Animation
       -> Raster
       -> Format
       -> Width
       -> Height
       -> FPS
       -> IO (Either String B.ByteString)
renderToMemory ani raster' format width height fps = do
  ffmpeg <- requireExecutable "ffmpeg"
  raster <- selectRaster raster'
  generateFrames raster ani width height fps $ \template ->
    case format of
      RenderMp4 ->
        runCmdLazy ffmpeg ["-r", show fps, "-i", template, "-y"
                      , "-c:v", "libx264", "-vf", "fps="++show fps
                      , "-preset", "slow"
                      , "-crf", "18"
                      , "-movflags", "+faststart"
                      , "-pix_fmt", "yuv420p"
                      , "-f", "ismv"
                      ,  "-"] Prelude.id
      RenderGif -> withTempFile "png" $ \palette -> do
        runCmd_ ffmpeg ["-i", template, "-y"
                      ,"-vf", "fps="++show fps++
                        ",scale="++show width++":"++show height ++
                        ":flags=lanczos,palettegen"
                      ,"-t", showFFloat Nothing (duration ani) ""
                      , palette ]

        runCmdLazy ffmpeg ["-framerate", show fps,"-i", template, "-y"
                      ,"-i", palette
                      ,"-threads", "0"
                      ,"-filter_complex"
                      ,"fps="++show fps++
                        ",scale="++show width++":"++show height ++
                        ":flags=lanczos[x];[x][1:v]paletteuse"
                      ,"-t", showFFloat Nothing (duration ani) ""
                      , "-f", "gif"
                      , "-"] Prelude.id
      RenderWebm ->
        runCmdLazy ffmpeg ["-r", show fps, "-i", template, "-y"
                      , "-c:v", "libvpx-vp9", "-vf", "fps="++show fps
                      , "-threads", "0"
                      , "-f", "webm"
                      , "-"] Prelude.id

rasterTemplate :: Raster -> String
rasterTemplate RasterNone = "render-%05d.svg"
rasterTemplate _          = "render-%05d.png"

frameOrder :: Int -> Int -> [Int]
frameOrder fps nFrames = worker [] fps
  where
    worker _seen 0 = []
    worker seen nthFrame =
      filterFrameList seen nthFrame nFrames ++
      worker (nthFrame : seen) (nthFrame `div` 2)

filterFrameList :: [Int] -> Int -> Int -> [Int]
filterFrameList seen nthFrame nFrames =
    filter (not.isSeen) [0, nthFrame .. nFrames-1]
  where
    isSeen x = any (\y -> x `mod` y == 0) seen

generateFrames :: Raster -> Animation -> Width -> Height -> FPS -> (String -> IO (Either String B.ByteString)) -> IO (Either String B.ByteString)
generateFrames raster ani width_ height_ rate action = withTempDir $ \tmp -> do
    setRootDirectory tmp
    done <- newMVar (0::Int)
    let frameName nth = tmp </> printf nameTemplate nth
    start <- getCurrentTime
    concurrentForM_ frames $ \n -> do
      writeFile (frameName n) $ renderSvg width height $ nthFrame n
      applyRaster raster (frameName n)
      modifyMVar_ done $ \nDone -> return (nDone+1)
    now <- getCurrentTime
    let spent = diffUTCTime now start
    action (tmp </> rasterTemplate raster)
  where
    width = Just $ Px $ fromIntegral width_
    height = Just $ Px $ fromIntegral height_
    -- frames = [0..frameCount-1]
    frames = frameOrder rate frameCount
    nthFrame nth = frameAt (recip (fromIntegral rate) * fromIntegral nth) ani
    frameCount = round (duration ani * fromIntegral rate) :: Int
    nameTemplate :: String
    nameTemplate = "render-%05d.svg"

concurrentForM_ :: [a] -> (a -> IO ()) -> IO ()
concurrentForM_ lst action = do
  n <- getNumCapabilities
  sem <- newQSemN n
  eVar <- newEmptyMVar
  forM_ lst $ \elt -> do
    waitQSemN sem 1
    emp <- isEmptyMVar eVar
    if emp
      then void $ forkIO (catch (action elt) (void . tryPutMVar eVar) `finally` signalQSemN sem 1)
      else signalQSemN sem 1
  waitQSemN sem n
  mbE <- tryTakeMVar eVar
  case mbE of
    Nothing -> return ()
    Just e  -> throwIO (e :: SomeException)

runCmd_ :: FilePath -> [String] -> IO (Either String String)
runCmd_ exec args = do
  (ret, stdout, stderr) <- readProcessWithExitCode exec args ""
  _ <- evaluate (length stdout + length stderr)
  case ret of
    ExitSuccess -> return (Right stdout)
    ExitFailure err | False ->
      return $ Left $
        "Failed to run: " ++ showCommandForUser exec args ++ "\n" ++
        "Error code: " ++ show err ++ "\n" ++
        "stderr: " ++ stderr
    ExitFailure{} | null stderr -> -- LaTeX prints errors to stdout. :(
      return $ Left stdout
    ExitFailure{} ->
      return $ Left stderr

runCmdLazy :: FilePath -> [String] -> (IO (Either String B.ByteString) -> IO a) -> IO a
runCmdLazy exec args handler = do
  (inp, out, err, pid) <- runInteractiveProcess exec args Nothing Nothing
  hSetBinaryMode out True
  hClose inp
  let fetch = do
        eof <- hIsEOF out
        if eof
          then do
            stderr <- hGetContents err
            _ <- evaluate (length stderr)
            ret <- waitForProcess pid
            case ret of
              ExitSuccess   -> return (Left "")
              ExitFailure{} -> return (Left stderr)
          else do
            Right <$> B.hGetContents out
  handler fetch `finally` do
    terminateProcess pid
    _ <- waitForProcess pid
    return ()
