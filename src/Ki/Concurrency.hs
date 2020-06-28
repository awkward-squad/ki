{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Ki.Concurrency
  ( IO,
    MVar,
    STM,
    TMVar,
    TVar,
    ThreadId,
    atomically,
    catch,
    Ki.Concurrency.forkIO,
    modifyTVar',
    myThreadId,
    newEmptyTMVarIO,
    newMVar,
    newTVar,
    newTVarIO,
    putTMVar,
    readTMVar,
    readTVar,
    Ki.Concurrency.registerDelay,
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

#ifdef TEST

import Control.Concurrent.Classy (atomically, catch, fork, modifyTVar', myThreadId, newEmptyTMVar, newMVar, newTVar, putTMVar, readTMVar, readTVar, retry, throw, throwSTM, uninterruptibleMask, unsafeUnmask, withMVar, writeTVar)
import qualified Control.Concurrent.Classy
import Control.Exception (Exception)
import Test.DejaFu
import Test.DejaFu.Conc.Internal.Common
import Test.DejaFu.Conc.Internal.STM
import Test.DejaFu.Types (ThreadId)
import qualified Prelude
import Prelude hiding (IO)

type IO =
  ConcIO

type MVar =
  ModelMVar Prelude.IO

type STM =
  ModelSTM Prelude.IO

type TMVar =
  Control.Concurrent.Classy.TMVar STM

type TVar =
  ModelTVar Prelude.IO

forkIO :: IO () -> IO ThreadId
forkIO = fork

newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = atomically newEmptyTMVar

newTVarIO :: a -> IO (TVar a)
newTVarIO = atomically . newTVar

registerDelay :: Int -> IO (STM (), IO ())
registerDelay micros = do
  var <- Control.Concurrent.Classy.registerDelay micros
  pure (readTVar var >>= \b -> if b then pure () else retry, pure ())

throwIO :: Exception e => e -> IO a
throwIO =
  throw

throwTo :: Exception e => ThreadId -> e -> IO ()
throwTo =
  Control.Concurrent.Classy.throwTo

try :: Exception e => IO a -> IO (Either e a)
try action =
  catch (Right <$> action) (pure . Left)

uniqueInt :: IO Int
uniqueInt =
  pure 0

#else

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (unless)
import Data.Atomics.Counter
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

#endif
