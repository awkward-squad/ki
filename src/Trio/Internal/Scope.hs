{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trio.Internal.Scope where

import Control.Concurrent.Classy.STM
import Control.Monad
import Control.Monad.Conc.Class
import Data.Set (Set)
import qualified Data.Set as Set
import Trio.Internal.Conc (blockUntilTVar)
-- import Trio.Internal.Debug

type Scope m =
  TMVar (STM m) (ScopeState m)

data ScopeState m = ScopeState
  { -- | Running children.
    runningVar :: TVar (STM m) (Set (ThreadId m)),
    -- | Number of children that are just about to start.
    startingVar :: TVar (STM m) Int
  }

newScope :: MonadConc m => m (Scope m)
newScope =
  atomically do
    runningVar <- newTVar Set.empty
    startingVar <- newTVar 0
    newTMVar
      ScopeState
        { runningVar,
          startingVar
        }

softCloseScope :: MonadConc m => Scope m -> m ()
softCloseScope scopeVar = do
  scope <- atomically (readTMVar scopeVar)
  atomically do
    blockUntilTVar (runningVar scope) Set.null
    blockUntilTVar (startingVar scope) (== 0)
    void (takeTMVar scopeVar)
