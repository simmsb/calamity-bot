-- |
module Polysemy.Timeout
  ( Timeout (..),
    timeout,
    timeoutDuration,
    timeoutToIOFinal,
  )
where

import qualified System.Timeout as T
import Polysemy
import Polysemy.Final
import Data.Hourglass (toSeconds, Duration)

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
    fmap (fmap join) <$> (liftS $ T.timeout time (inspect ins <$> fm'))
