{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trio.Internal.Nursery where

import Control.Concurrent.Classy.STM
import Control.Monad
import Control.Monad.Conc.Class
import Data.Set (Set)
import qualified Data.Set as Set
import Trio.Internal.Conc (blockUntilTVar)
-- import Trio.Internal.Debug

type Nursery m =
  TMVar (STM m) (NurseryState m)

data NurseryState m = NurseryState
  { -- | Running children.
    runningVar :: TVar (STM m) (Set (ThreadId m)),
    -- | Number of children that are just about to start.
    startingVar :: TVar (STM m) Int
  }

newNursery :: MonadConc m => m (Nursery m)
newNursery =
  atomically do
    runningVar <- newTVar Set.empty
    startingVar <- newTVar 0
    newTMVar
      NurseryState
        { runningVar,
          startingVar
        }

softCloseNursery :: MonadConc m => Nursery m -> m ()
softCloseNursery nurseryVar = do
  nursery <- atomically (readTMVar nurseryVar)
  atomically do
    blockUntilTVar (runningVar nursery) Set.null
    blockUntilTVar (startingVar nursery) (== 0)
    void (takeTMVar nurseryVar)

-- | Close a nursery, preventing any more children from spawning. Those that
-- do attempt to use the nursery afterwards will be delivered a 'NurseryClosed'
-- exception. Returns the children that are still running.
hardCloseNursery ::
  MonadConc m =>
  Nursery m ->
  STM m (TVar (STM m) (Set (ThreadId m)))
hardCloseNursery nurseryVar =
  tryTakeTMVar nurseryVar >>= \case
    Nothing -> newTVar Set.empty
    Just nursery -> do
      starting <- readTVar (startingVar nursery)
      unless (starting == 0) retry
      pure (runningVar nursery)
