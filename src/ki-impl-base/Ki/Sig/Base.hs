{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Ki.Sig.Base
  ( IO,
    STM,
    TVar,
    TMVar,
    ThreadId,
    atomically,
    catch,
    Ki.Sig.Base.forkIO,
    modifyTVar',
    myThreadId,
    Ki.Sig.Base.newEmptyTMVar,
    Ki.Sig.Base.newTVar,
    putTMVar,
    readTMVar,
    readTVar,
    -- registerDelay,
    retry,
    throwIO,
    throwSTM,
    throwTo,
    try,
    uninterruptibleMask,
    unsafeUnmask,
    writeTVar,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import GHC.Conc
import GHC.Exts (fork#)
import GHC.IO

forkIO :: IO () -> IO ThreadId
forkIO action =
  IO \s ->
    case fork# action s of
      (# s1, tid #) -> (# s1, ThreadId tid #)

newEmptyTMVar :: String -> STM (TMVar a)
newEmptyTMVar _ =
  Control.Concurrent.STM.newEmptyTMVar

newTVar :: String -> a -> STM (TVar a)
newTVar _ =
  Control.Concurrent.STM.newTVar
