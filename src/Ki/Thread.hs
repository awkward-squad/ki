module Ki.Thread
  ( Thread,
    async,
    asyncWithUnmask,
    await,
    awaitSTM,
    awaitFor,
    kill,
    fork,
    forkWithUnmask,
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (fromException))
import Ki.AsyncThreadFailed (AsyncThreadFailed (AsyncThreadFailed))
import Ki.Context (Context)
import qualified Ki.Context
import Ki.Duration (Duration)
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope
import Ki.Timeout (timeoutSTM)

-- | A running __thread__.
data Thread a = Thread
  { threadId :: !ThreadId,
    action :: !(STM a)
  }
  deriving stock (Functor, Generic)

instance Eq (Thread a) where
  Thread id1 _ == Thread id2 _ =
    id1 == id2

instance Ord (Thread a) where
  compare (Thread id1 _) (Thread id2 _) =
    compare id1 id2

-- | Fork a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> IO a -> IO (Thread (Either SomeException a))
async scope action =
  asyncWithRestore scope \restore -> restore action

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either SomeException a))
asyncWithUnmask scope action =
  asyncWithRestore scope \restore -> restore (action unsafeUnmask)

asyncWithRestore :: forall a. Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either SomeException a))
asyncWithRestore scope action = do
  resultVar <- newEmptyTMVarIO
  childThreadId <- Ki.Scope.fork scope action (putTMVarIO resultVar)
  pure (Thread childThreadId (readTMVar resultVar))

-- | Wait for a __thread__ to finish.
await :: Thread a -> IO a
await =
  atomically . awaitSTM

-- | @STM@ variant of 'await'.
awaitSTM :: Thread a -> STM a
awaitSTM Thread {action} =
  action

-- | Variant of 'await' that gives up after the given duration.
--
-- @
-- 'awaitFor' thread duration =
--   'timeout' duration (pure . Just \<$\> 'awaitSTM' thread) (pure Nothing)
-- @
awaitFor :: Thread a -> Duration -> IO (Maybe a)
awaitFor thread duration =
  timeoutSTM duration (pure . Just <$> awaitSTM thread) (pure Nothing)

-- | Kill a __thread__ wait for it to finish.
--
-- /Throws/:
--
--   * 'ThreadKilled' if a __thread__ attempts to kill itself.
kill :: Thread a -> IO ()
kill thread = do
  throwTo (threadId thread) ThreadKilled
  void (await thread)

-- | Variant of 'async' that does not return a handle to the __thread__.
--
-- If the __thread__ throws an exception, the exception is propagated up the call tree to the __thread__ that opened its
-- __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: Scope -> IO a -> IO (Thread a)
fork scope action =
  forkWithRestore scope \restore -> restore action

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
forkWithUnmask scope action =
  forkWithRestore scope \restore -> restore (action unsafeUnmask)

forkWithRestore :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
forkWithRestore scope action = do
  parentThreadId <- myThreadId
  resultVar <- newEmptyTMVarIO
  childThreadId <-
    Ki.Scope.fork scope action \result -> do
      whenLeft result \exception -> do
        whenM
          (shouldPropagateException (Ki.Scope.context scope) exception)
          (throwTo parentThreadId (AsyncThreadFailed exception))
      putTMVarIO resultVar result
  pure (Thread childThreadId (readTMVar resultVar >>= either throwSTM pure))

shouldPropagateException :: Context -> SomeException -> IO Bool
shouldPropagateException context exception =
  case fromException exception of
    Just ThreadKilled -> pure False
    Just _ -> pure True
    Nothing ->
      case fromException exception of
        Just (Ki.Context.Cancelled token) -> atomically ((/= token) <$> Ki.Context.cancelled context <|> pure True)
        Nothing -> pure True
