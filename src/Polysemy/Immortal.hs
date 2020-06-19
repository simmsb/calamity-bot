-- |
module Polysemy.Immortal
  ( Immortal (..),
    createImmortal,
    mortalize,
    immortalize,
    stop,
    wait,
    immortalToIOFinal,
  )
where

import qualified Control.Immortal as Immortal
import Polysemy
import Polysemy.Final

data Immortal m a where
  CreateImmortal :: (Immortal.Thread -> m a) -> Immortal m Immortal.Thread
  Mortalize :: Immortal.Thread -> Immortal m ()
  Immortalize :: Immortal.Thread -> Immortal m ()
  Stop :: Immortal.Thread -> Immortal m ()
  Wait :: Immortal.Thread -> Immortal m ()

makeSem ''Immortal

immortalToIOFinal ::
  Member (Final IO) r =>
  Sem (Immortal ': r) a ->
  Sem r a
immortalToIOFinal = interpretFinal $ \case
  CreateImmortal fm -> do
    ins <- getInspectorS
    s <- getInitialStateS
    fm' <- bindS fm
    liftS $ Immortal.create (\t -> void $ inspect ins <$> fm' (s $> t))
  Mortalize t -> liftS $ Immortal.immortalize t
  Immortalize t -> liftS $ Immortal.immortalize t
  Stop t -> liftS $ Immortal.stop t
  Wait t -> liftS $ Immortal.wait t
