module Polysemy.Timeout (
  Timeout (..),
  timeout,
  timeoutDuration,
  timeoutToIOFinal,
) where

import Control.Monad
import Data.Hourglass (Duration, toSeconds)
import Polysemy
import Polysemy.Final
import System.Timeout qualified

data Timeout m a where
  Timeout :: Int -> m a -> Timeout m (Maybe a)

makeSem ''Timeout

timeoutDuration :: Member Timeout r => Duration -> Sem r a -> Sem r (Maybe a)
timeoutDuration d = timeout (fromInteger . (* 10 ^ (6 :: Integer)) . toInteger . toSeconds $ d)

timeoutToIOFinal ::
  Member (Final IO) r =>
  Sem (Timeout ': r) a ->
  Sem r a
timeoutToIOFinal = interpretFinal $ \case
  Timeout time fm -> do
    ins <- getInspectorS
    fm' <- runS fm
    fmap (fmap join) <$> liftS (System.Timeout.timeout time (inspect ins <$> fm'))
