{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Ki.Sig.Base
  ( IO,
    STM,
    TMVar,
    TVar,
    ThreadId,
    atomically,
    catch,
    Ki.Sig.Base.forkIO,
    modifyTVar',
    myThreadId,
    newEmptyTMVarIO,
    Ki.Sig.Base.newTVar,
    Ki.Sig.Base.newUnique,
    putTMVar,
    readTMVar,
    readTVar,
    Ki.Sig.Base.registerDelay,
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
import Control.Exception
import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.IORef
import GHC.Conc
#if defined(mingw32_HOST_OS)
import GHC.Conc.Windows
#else
import GHC.Event
#endif
import GHC.Exts (fork#)
import GHC.IO

newtype TMVar a
  = TMVar (TVar (Maybe a))

forkIO :: IO () -> IO ThreadId
forkIO action =
  IO \s ->
    case fork# action s of
      (# s1, tid #) -> (# s1, ThreadId tid #)

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
  x <- readTVar var
  writeTVar var $! f x

newEmptyTMVarIO :: forall a. String -> IO (TMVar a)
newEmptyTMVarIO _ =
  coerce @(IO (TVar (Maybe a))) (GHC.Conc.newTVarIO Nothing)

newTVar :: String -> a -> STM (TVar a)
newTVar _ =
  GHC.Conc.newTVar

newUnique :: IO Integer
newUnique =
  atomicModifyIORef' uniqueRef \n -> let m = n + 1 in (m, m)

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar var) x =
  readTVar var >>= \case
    Nothing -> writeTVar var (Just x)
    Just _ -> retry

readTMVar :: TMVar a -> STM a
readTMVar (TMVar var) =
  readTVar var >>= \case
    Nothing -> retry
    Just x -> pure x

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

uniqueRef :: IORef Integer
uniqueRef =
  unsafePerformIO (newIORef 0)
{-# NOINLINE uniqueRef #-}
