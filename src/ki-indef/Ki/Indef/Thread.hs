module Ki.Indef.Thread
  ( Thread (..),
    await,
    awaitSTM,
    kill,
    ThreadFailed (..),

    -- * Internal API
    AsyncThreadFailed (..),
    translateAsyncThreadFailed,
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (..), SomeException, asyncExceptionFromException, asyncExceptionToException)
import Data.Functor (void)
import Ki.Sig (IO, STM, TMVar, ThreadId, atomically, readTMVar, throwSTM, throwTo)
import Prelude hiding (IO)

-- | A running __thread__.
data Thread a
  = Thread
      !ThreadId
      !(TMVar (Either SomeException a))

-- | Wait for a __thread__ to finish.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception.
await :: Thread a -> IO a
await =
  atomically . awaitSTM

-- | @STM@ variant of 'await'.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception.
awaitSTM :: Thread a -> STM a
awaitSTM (Thread threadId resultVar) =
  readTMVar resultVar >>= \case
    Left exception -> throwSTM (ThreadFailed threadId exception)
    Right result -> pure result

-- | Kill a __thread__ wait for it to finish.
--
-- /Throws/:
--
--   * 'ThreadKilled' if a __thread__ attempts to kill itself.
kill :: Thread a -> IO ()
kill (Thread threadId resultVar) = do
  throwTo threadId ThreadKilled
  void (atomically (readTMVar resultVar))

data ThreadFailed
  = ThreadFailed !ThreadId !SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Unexported async variant of 'ThreadFailed'.
data AsyncThreadFailed
  = AsyncThreadFailed !ThreadId !SomeException
  deriving stock (Show)

instance Exception AsyncThreadFailed where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

translateAsyncThreadFailed :: SomeException -> SomeException
translateAsyncThreadFailed ex =
  case fromException ex of
    Just (AsyncThreadFailed threadId exception) ->
      toException (ThreadFailed threadId exception)
    _ -> ex
