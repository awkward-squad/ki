module Trio.Internal.Semaphore
  ( new
  , signal
  , wait
  ) where

import Control.Monad.Conc.Class


new :: MonadConc m => m ( MVar m () )
new = newEmptyMVar

signal :: MonadConc m => MVar m () -> m ()
signal v = putMVar v ()

wait :: MonadConc m => MVar m () -> m ()
wait = takeMVar
