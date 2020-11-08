module Ki.Thread
  ( Thread,
    async,
    asyncWithUnmask,
    await,
    awaitSTM,
    awaitFor,
    kill,
    fork,
    fork_,
    forkWithUnmask,
    forkWithUnmask_,
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (fromException))
import Data.Bifunctor (first)
import qualified Ki.Context as Context
import Ki.Duration (Duration)
import Ki.Prelude
import Ki.Scope (Scope (Scope))
import qualified Ki.Scope as Scope
import Ki.ScopeClosing (ScopeClosing (ScopeClosing))
import Ki.ThreadFailed (ThreadFailed (ThreadFailed), ThreadFailedAsync (ThreadFailedAsync))
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

-- | Create a __thread__ within a __scope__ to compute a value concurrently.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> IO a -> IO (Thread (Either ThreadFailed a))
async scope action =
  asyncWithRestore scope \restore -> restore action

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either ThreadFailed a))
asyncWithUnmask scope action =
  asyncWithRestore scope \restore -> restore (action unsafeUnmask)

asyncWithRestore :: forall a. Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either ThreadFailed a))
asyncWithRestore scope action = do
  resultVar <- newEmptyTMVarIO
  childThreadId <-
    Scope.scopeFork scope action \childThreadId result ->
      putTMVarIO resultVar (first (ThreadFailed childThreadId) result)
  pure (Thread childThreadId (readTMVar resultVar))

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

-- | Create a __thread__ within a __scope__ to compute a value concurrently.
--
-- If the __thread__ throws an exception, the exception is wrapped in 'ThreadFailed' and immediately propagated up the
-- call tree to the __thread__ that opened its __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: Scope -> IO a -> IO (Thread a)
fork scope action =
  forkWithRestore scope \restore -> restore action

-- | Variant of 'fork' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ :: Scope -> IO () -> IO ()
fork_ scope action =
  forkWithRestore_ scope \restore -> restore action

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
forkWithUnmask scope action =
  forkWithRestore scope \restore -> restore (action unsafeUnmask)

-- | Variant of 'forkWithUnmask' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask_ scope action =
  forkWithRestore_ scope \restore -> restore (action unsafeUnmask)

forkWithRestore :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
forkWithRestore scope action = do
  parentThreadId <- myThreadId
  resultVar <- newEmptyTMVarIO
  childThreadId <-
    Scope.scopeFork scope action \childThreadId -> \case
      Left exception -> do
        whenM
          (shouldPropagateException scope exception)
          (throwTo parentThreadId (ThreadFailedAsync threadFailedException))
        putTMVarIO resultVar (Left threadFailedException)
        where
          threadFailedException :: ThreadFailed
          threadFailedException =
            ThreadFailed childThreadId exception
      Right result -> putTMVarIO resultVar (Right result)
  pure (Thread childThreadId (readTMVar resultVar >>= either throwSTM pure))

forkWithRestore_ :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithRestore_ scope action = do
  parentThreadId <- myThreadId
  _childThreadId <-
    Scope.scopeFork scope action \childThreadId ->
      onLeft \exception -> do
        whenM
          (shouldPropagateException scope exception)
          (throwTo parentThreadId (ThreadFailedAsync (ThreadFailed childThreadId exception)))
  pure ()

shouldPropagateException :: Scope -> SomeException -> IO Bool
shouldPropagateException Scope {closedVar, context} exception =
  case fromException exception of
    -- Our scope is (presumably) closing, so don't propagate this exception that presumably just came from our parent.
    -- But if our scope's closedVar isn't True, that means this 'ScopeClosing' definitely came from somewhere else...
    Just ScopeClosing -> not <$> readTVarIO closedVar
    Nothing ->
      case fromException exception of
        -- We (presumably) are honoring our own cancellation request, so don't propagate that either.
        -- It's a bit complicated looking because we *do* want to throw this token if we (somehow) threw it
        -- "inappropriately" in the sense that it wasn't ours to throw - it was smuggled from elsewhere.
        Just token -> atomically ((/= token) <$> Context.contextCancelTokenSTM context <|> pure True)
        Nothing -> pure True
