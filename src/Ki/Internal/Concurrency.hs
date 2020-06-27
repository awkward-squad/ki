{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Ki.Internal.Concurrency
  ( IO,
    MVar,
    STM,
    TMVar,
    TVar,
    ThreadId,
    atomically,
    catch,
    Ki.Internal.Concurrency.forkIO,
    modifyTVar',
    myThreadId,
    newEmptyTMVarIO,
    newMVar,
    newTVar,
    newTVarIO,
    newUnique,
    putTMVar,
    readTMVar,
    readTVar,
    Ki.Internal.Concurrency.registerDelay,
    retry,
    throwIO,
    throwSTM,
    throwTo,
    try,
    uninterruptibleMask,
    uniqueInt,
    unsafeUnmask,
    withMVar,
    writeTVar,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (unless)
import Data.Atomics.Counter
import Data.Unique
import GHC.Conc (ThreadId (ThreadId))
#if defined(mingw32_HOST_OS)
import GHC.Conc.Windows
#else
import GHC.Event
#endif
import GHC.Exts (fork#)
import GHC.IO
import Prelude

forkIO :: IO () -> IO ThreadId
forkIO action =
  IO \s ->
    case fork# action s of
      (# s1, tid #) -> (# s1, ThreadId tid #)

#if defined(mingw32_HOST_OS)
registerDelay :: Int -> IO (STM (), IO ())
registerDelay micros = do
  var <- GHC.Conc.Windows.registerDelay micros
  pure (readTVar var >>= \b -> unless b retry, pure ()) -- no unregister on Windows =P
#else
registerDelay :: Int -> IO (STM (), IO ())
registerDelay micros = do
  var <- newTVarIO False
  manager <- getSystemTimerManager
  key <- registerTimeout manager micros (atomically (writeTVar var True))
  pure (readTVar var >>= \b -> unless b retry, unregisterTimeout manager key)
#endif

uniqueInt :: IO Int
uniqueInt =
  incrCounter 1 counter

counter :: AtomicCounter
counter =
  unsafePerformIO (newCounter 0)
{-# NOINLINE counter #-}
