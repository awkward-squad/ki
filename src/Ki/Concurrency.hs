{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Ki.Concurrency
  ( IO,
    MVar,
    STM,
    TBQueue,
    TMVar,
    TQueue,
    TVar,
    ThreadId,
    atomically,
    catch,
    check,
    forkIO,
    modifyTVar',
    myThreadId,
    newEmptyTMVarIO,
    newMVar,
    newTBQueueIO,
    newTQueueIO,
    newTVar,
    newTVarIO,
    onException,
    putTMVar,
    putTMVarIO,
    readTBQueue,
    readTMVar,
    readTQueue,
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
    writeTQueue,
    writeTVar,
  )
where

#ifdef TEST

import Control.Concurrent.Classy (atomically, catch, check, modifyTVar', myThreadId, newMVar, newTVar, putTMVar, readTBQueue, readTQueue, writeTQueue, readTMVar, readTVar, retry, throwSTM, uninterruptibleMask, unsafeUnmask, withMVar, writeTBQueue, writeTVar)
import qualified Control.Concurrent.Classy
import Control.Exception (Exception, SomeException)
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

type TQueue =
  Control.Concurrent.Classy.TQueue STM

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

newTQueueIO :: IO (TQueue a)
newTQueueIO =
  Control.Concurrent.Classy.atomically Control.Concurrent.Classy.newTQueue

newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO =
  Control.Concurrent.Classy.atomically Control.Concurrent.Classy.newEmptyTMVar

newTVarIO :: a -> IO (TVar a)
newTVarIO =
  Control.Concurrent.Classy.atomically . Control.Concurrent.Classy.newTVar

onException :: IO a -> IO b -> IO a
onException action cleanup =
  Control.Concurrent.Classy.catch action \exception -> do
    _ <- cleanup
    throwIO (exception :: SomeException)

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO var x =
  atomically (putTMVar var x)

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
import GHC.IO (IO (IO), unsafePerformIO, unsafeUnmask)
import Prelude

forkIO :: IO () -> IO ThreadId
forkIO action =
  IO \s ->
    case fork# action s of
      (# s1, tid #) -> (# s1, ThreadId tid #)

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO var x =
  atomically (putTMVar var x)

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
