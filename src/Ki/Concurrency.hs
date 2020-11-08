{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
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
    readTVarIO,
    registerDelay,
    retry,
    threadDelay,
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

import Control.Concurrent.Classy hiding (IO, MVar, STM, TBQueue, TMVar, TQueue, TVar, ThreadId, registerDelay)
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
  fork

newTBQueueIO :: Natural -> IO (TBQueue a)
newTBQueueIO =
  atomically . newTBQueue

newTQueueIO :: IO (TQueue a)
newTQueueIO =
  atomically newTQueue

newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO =
  atomically newEmptyTMVar

newTVarIO :: a -> IO (TVar a)
newTVarIO =
  atomically . newTVar

onException :: IO a -> IO b -> IO a
onException action cleanup =
  catch @_ @SomeException action \exception -> do
    _ <- cleanup
    throwIO exception

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO var x =
  atomically (putTMVar var x)

readTVarIO :: TVar a -> IO a
readTVarIO =
  atomically . readTVar

registerDelay :: Int -> IO (STM (), IO ())
registerDelay micros = do
  var <- Control.Concurrent.Classy.registerDelay micros
  pure (readTVar var >>= check, pure ())

throwIO :: Exception e => e -> IO a
throwIO =
  throw

try :: Exception e => IO a -> IO (Either e a)
try action =
  catch (Right <$> action) (pure . Left)

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
