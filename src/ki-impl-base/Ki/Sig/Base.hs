module Ki.Sig.Base
  ( IO,
    STM,
    TVar,
    TMVar,
    ThreadId,
    atomically,
    catch,
    forkIO,
    modifyTVar',
    myThreadId,
    Ki.Sig.Base.newEmptyTMVar,
    Ki.Sig.Base.newTVar,
    putTMVar,
    readTMVar,
    readTVar,
    registerDelay,
    retry,
    throwIO,
    throwSTM,
    throwTo,
    try,
    uninterruptibleMask,
    uninterruptibleMask_,
    unsafeUnmask,
    writeTVar,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import GHC.IO

newEmptyTMVar :: String -> STM (TMVar a)
newEmptyTMVar _ =
  Control.Concurrent.STM.newEmptyTMVar

newTVar :: String -> a -> STM (TVar a)
newTVar _ =
  Control.Concurrent.STM.newTVar
