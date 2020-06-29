{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Ki.Concurrency
  ( IO,
    MVar,
    STM,
    TBQueue,
    TMVar,
    TVar,
    ThreadId,
    atomically,
    catch,
    forkIO,
    modifyTVar',
    myThreadId,
    newEmptyTMVarIO,
    newMVar,
    newTBQueueIO,
    newTVar,
    newTVarIO,
    putTMVar,
    readTBQueue,
    readTMVar,
    readTVar,
    registerDelay,
    retry,
    throwIO,
    throwSTM,
    throwTo,
    try,
    uninterruptibleMask,
    uniqueInt,
    unsafeUnmask,
    withMVar,
    writeTBQueue,
    writeTVar,
  )
where

#ifdef TEST

import Control.Concurrent.Classy (atomically, catch, modifyTVar', myThreadId, newMVar, newTVar, putTMVar, readTBQueue, readTMVar, readTVar, retry, throwSTM, uninterruptibleMask, unsafeUnmask, withMVar, writeTBQueue, writeTVar)
import qualified Control.Concurrent.Classy
import Control.Exception (Exception)
import Numeric.Natural (Natural)
import qualified Test.DejaFu
import qualified Test.DejaFu.Conc.Internal.Common
import qualified Test.DejaFu.Conc.Internal.STM
import Test.DejaFu.Types (ThreadId)
import qualified Prelude
import Prelude hiding (IO)

type IO =
  Test.DejaFu.ConcIO

type MVar =
  Test.DejaFu.Conc.Internal.Common.ModelMVar Prelude.IO

type STM =
  Test.DejaFu.Conc.Internal.STM.ModelSTM Prelude.IO

type TBQueue =
  Control.Concurrent.Classy.TBQueue STM

type TMVar =
  Control.Concurrent.Classy.TMVar STM

type TVar =
  Test.DejaFu.Conc.Internal.STM.ModelTVar Prelude.IO

forkIO :: IO () -> IO ThreadId
forkIO =
  Control.Concurrent.Classy.fork

newTBQueueIO :: Natural -> IO (TBQueue a)
newTBQueueIO =
  Control.Concurrent.Classy.atomically . Control.Concurrent.Classy.newTBQueue

newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO =
  Control.Concurrent.Classy.atomically Control.Concurrent.Classy.newEmptyTMVar

newTVarIO :: a -> IO (TVar a)
newTVarIO =
  Control.Concurrent.Classy.atomically . Control.Concurrent.Classy.newTVar

registerDelay :: Int -> IO (STM (), IO ())
registerDelay micros = do
  var <- Control.Concurrent.Classy.registerDelay micros
  pure (Control.Concurrent.Classy.readTVar var >>= Control.Concurrent.Classy.check, pure ())

throwIO :: Exception e => e -> IO a
throwIO =
  Control.Concurrent.Classy.throw

throwTo :: Exception e => ThreadId -> e -> IO ()
throwTo =
  Control.Concurrent.Classy.throwTo

try :: Exception e => IO a -> IO (Either e a)
try action =
  Control.Concurrent.Classy.catch (Right <$> action) (pure . Left)

uniqueInt :: IO Int
uniqueInt =
  pure 0

#else

import Control.Concurrent hiding (forkIO)
import Control.Concurrent.STM hiding (registerDelay)
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
