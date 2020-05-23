module Ki.Indef.Thread
  ( Thread (..),
    await,
    awaitSTM,
    awaitFor,
    kill,
    --
    timeout,

    -- * Internal API
    AsyncThreadFailed (..),
    unwrapAsyncThreadFailed,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (AsyncException (ThreadKilled), Exception (..), SomeException, asyncExceptionFromException, asyncExceptionToException)
import Control.Monad (join)
import Data.Functor (($>), void)
import GHC.Generics (Generic)
import Ki.Indef.Seconds (Seconds)
import qualified Ki.Indef.Seconds as Seconds
import Ki.Sig (IO, STM, TMVar, ThreadId, atomically, readTMVar, registerDelay, throwSTM, throwTo)
import Prelude hiding (IO)

-- | A running __thread__.
data Thread a
  = Thread
      !ThreadId
      !(TMVar (Either SomeException a))
  deriving stock (Generic)

instance Eq (Thread a) where
  Thread id1 _ == Thread id2 _ =
    id1 == id2

instance Ord (Thread a) where
  compare (Thread id1 _) (Thread id2 _) =
    compare id1 id2

-- | Wait for a __thread__ to finish.
--
-- /Throws/:
--
--   * The exception that the __thread__ threw, if any.
await :: Thread a -> IO a
await =
  atomically . awaitSTM

-- | @STM@ variant of 'await'.
--
-- /Throws/:
--
--   * The exception that the __thread__ threw, if any.
awaitSTM :: Thread a -> STM a
awaitSTM (Thread _threadId resultVar) =
  readTMVar resultVar >>= \case
    Left exception -> throwSTM exception
    Right result -> pure result

-- | Variant of 'await' that gives up after the given number of seconds elapses.
--
-- @
-- 'awaitFor' thread seconds =
--   'timeout' seconds (pure . Just \<$\> 'awaitSTM' thread) (pure Nothing)
-- @
awaitFor :: Thread a -> Seconds -> IO (Maybe a)
awaitFor thread seconds =
  timeout seconds (pure . Just <$> awaitSTM thread) (pure Nothing)

-- | Kill a __thread__ wait for it to finish.
--
-- /Throws/:
--
--   * 'ThreadKilled' if a __thread__ attempts to kill itself.
kill :: Thread a -> IO ()
kill (Thread threadId resultVar) = do
  throwTo threadId ThreadKilled
  void (atomically (readTMVar resultVar))

newtype AsyncThreadFailed
  = AsyncThreadFailed SomeException
  deriving stock (Show)

instance Exception AsyncThreadFailed where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

unwrapAsyncThreadFailed :: SomeException -> SomeException
unwrapAsyncThreadFailed ex =
  case fromException ex of
    Just (AsyncThreadFailed exception) -> exception
    _ -> ex

-- Misc. utils

-- | Wait for an @STM@ action to return, and return the @IO@ action contained
-- within.
--
-- If the given number of seconds elapse, return the given @IO@ action instead.
timeout :: Seconds -> STM (IO a) -> IO a -> IO a
timeout seconds action fallback = do
  (delay, unregister) <- registerDelay (Seconds.toMicros seconds)
  join (atomically (delay $> fallback <|> (unregister >>) <$> action))
