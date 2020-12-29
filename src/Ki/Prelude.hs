{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Ki.Prelude
  ( atomicallyIO,
    forkIO,
    onLeft,
    putTMVarIO,
    registerDelay,
    uniqueInt,
    whenJust,
    whenLeft,
    whenM,
    module X,
  )
where

import Control.Applicative as X (optional, (<|>))
import Control.Concurrent hiding (forkIO)
import Control.Concurrent as X (ThreadId, myThreadId, threadDelay, throwTo)
import Control.Concurrent.STM as X hiding (registerDelay)
import Control.Exception
import Control.Exception as X (Exception, SomeException, throwIO, try, uninterruptibleMask)
import Control.Monad (unless)
import Control.Monad as X (join, unless)
import Control.Monad.IO.Class as X (MonadIO (..))
import Data.Coerce as X (coerce)
import Data.Foldable as X (for_)
import Data.Function as X (fix)
import Data.Functor as X (void, ($>), (<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.IntMap.Strict as X (IntMap)
import Data.Map.Strict as X (Map)
import Data.Maybe as X (fromMaybe)
import Data.Set as X (Set)
import Data.Word as X (Word32)
import GHC.Conc (ThreadId (ThreadId))
#if defined(mingw32_HOST_OS)
import GHC.Conc.Windows
#else
import GHC.Event
#endif
import GHC.Exts (fork#)
import GHC.Generics as X (Generic)
import GHC.IO (IO (IO), unsafePerformIO)
import GHC.IO as X (unsafeUnmask)
import Prelude as X

atomicallyIO :: STM (IO a) -> IO a
atomicallyIO =
  join . atomically

-- Control.Concurrent.forkIO without the dumb exception handler
forkIO :: IO () -> IO ThreadId
forkIO action =
  IO \s ->
    case fork# action s of
      (# s1, tid #) -> (# s1, ThreadId tid #)

onLeft :: (a -> IO b) -> Either a b -> IO b
onLeft f =
  either f pure

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
  atomicModifyIORef' counter \n -> let n' = n + 1 in (n', n')

counter :: IORef Int
counter =
  unsafePerformIO (newIORef 0)
{-# NOINLINE counter #-}

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust x f =
  maybe (pure ()) f x

whenLeft :: Either a b -> (a -> IO b) -> IO b
whenLeft x f =
  either f pure x

whenM :: IO Bool -> IO () -> IO ()
whenM x y =
  x >>= \case
    False -> pure ()
    True -> y
