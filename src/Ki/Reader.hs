-- | Please read "Ki.Documentation" for an overview of how to use this library.
--
-- This module exposes an API that uses a reader monad to pass around the __context__ implicitly. If you do not intend
-- to use soft-cancellation, you may want to use the simpler API exposed by "Ki".
module Ki.Reader
  ( -- * Context
    Context,
    globalContext,
    HasContext (..),

    -- * Scope
    Scope,
    scoped,
    scoped_,
    Ki.wait,
    waitSTM,
    waitFor,

    -- * Thread
    Thread,
    fork,
    fork_,
    forkWithUnmask,
    forkWithUnmask_,
    async,
    asyncWithUnmask,
    Ki.await,
    awaitSTM,
    awaitFor,

    -- * Soft-cancellation
    CancelToken,
    Ki.Implicit.cancel,
    cancelled,
    cancelledSTM,
    Cancelled (Cancelled),

    -- * Miscellaneous
    Duration,
    microseconds,
    milliseconds,
    seconds,
    timeoutSTM,
    sleep,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Ki
import qualified Ki.Implicit
import Ki.Internal.CancelToken (CancelToken)
import Ki.Internal.Context (Context, contextCancelToken, globalContext)
import Ki.Internal.Duration (Duration, microseconds, milliseconds, seconds)
import Ki.Internal.Prelude
import Ki.Internal.Scope (Cancelled (Cancelled), Scope, scopeScoped, scopeWaitSTM)
import Ki.Internal.Thread
  ( Thread (thread'Await),
    threadAsync,
    threadAsyncWithUnmask,
    threadAwaitFor,
    threadFork,
    threadForkWithUnmask,
    threadForkWithUnmask_,
    threadFork_,
  )
import Ki.Internal.Timeout (timeoutSTM)

-- | The class of reader monads that contain a __context__ in their environment.
class MonadUnliftIO m => HasContext m where
  -- | Project the __context__ from the environment.
  askContext :: m Context

  -- | Run an @m@ action, replacing its __context__ with the one provided.
  withContext :: Context -> m a -> m a

-- | Create a thread within a scope.
--
-- Reference manual: "Ki.Documentation#reference_manual_async"
async ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  m a ->
  m (Thread (Either SomeException a))
async scope action =
  threadAsync scope (with scope action)

-- | Variant of 'Ki.Reader.async' that provides the thread a function that unmasks asynchronous exceptions.
--
-- Reference manual: "Ki.Documentation#reference_manual_async"
asyncWithUnmask ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  ((forall x. m x -> m x) -> m a) ->
  m (Thread (Either SomeException a))
asyncWithUnmask scope action =
  threadAsyncWithUnmask scope \unmask -> with scope (action unmask)

-- | Variant of 'Ki.Reader.await' that gives up after the given duration.
awaitFor ::
  MonadIO m =>
  -- |
  Thread a ->
  -- |
  Duration ->
  m (Maybe a)
awaitFor =
  threadAwaitFor
{-# INLINE awaitFor #-}

-- | @STM@ variant of 'Ki.Reader.await'.
awaitSTM ::
  -- |
  Thread a ->
  STM a
awaitSTM =
  thread'Await

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The __cancel token__ may be
-- thrown to fulfill the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with
-- a value.
cancelled ::
  HasContext m =>
  -- |
  m (Maybe CancelToken)
cancelled =
  either Just (const Nothing) <$> cancelledSTM (pure ())

-- | @STM@ variant of 'Ki.Reader.cancelled'.
cancelledSTM ::
  HasContext m =>
  -- |
  STM a ->
  m (Either CancelToken a)
cancelledSTM action = do
  context <- askContext
  liftIO (atomically (Left <$> contextCancelToken context <|> Right <$> action))

-- | Create a child __thread__ within a __scope__.
--
-- If the child throws an exception, the exception is immediately propagated to its parent, unless the exception is a
-- __cancel token__ that originated from its parent's __scope__ being /cancelled/.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  m a ->
  m (Thread a)
fork scope action =
  threadFork scope (with scope action)

-- | Variant of 'Ki.Reader.fork' that does not return a handle to the child __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  m () ->
  m ()
fork_ scope action =
  threadFork_ scope (with scope action)

-- | Variant of 'Ki.Reader.fork' that provides the child __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  ((forall x. m x -> m x) -> m a) ->
  m (Thread a)
forkWithUnmask scope action =
  threadForkWithUnmask scope \unmask -> with scope (action unmask)

-- | Variant of 'Ki.Reader.forkWithUnmask' that does not return a handle to the child __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  ((forall x. m x -> m x) -> m ()) ->
  m ()
forkWithUnmask_ scope action =
  threadForkWithUnmask_ scope \unmask -> with scope (action unmask)

-- | Open a __scope__, perform an action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- The __scope__'s __context__ may become /cancelled/; if it does, and the provided action fulfills the __cancellation__
-- request by throwing the corresponding __cancel token__, this function will return 'Ki.Reader.Cancelled'.
--
-- ==== __Examples__
--
-- @
-- 'Ki.Reader.scoped' \\scope -> do
--   'Ki.Reader.fork_' scope worker1
--   'Ki.Reader.fork_' scope worker2
--   'Ki.Reader.wait' scope
-- @
scoped ::
  HasContext m =>
  -- |
  (Scope -> m a) ->
  -- |
  m (Either Cancelled a)
scoped action = do
  context <- askContext
  scopeScoped context \scope -> with scope (action scope)

-- | Variant of 'Ki.Reader.scoped' that throws 'Ki.Reader.Cancelled' instead of returning it.
scoped_ ::
  HasContext m =>
  -- |
  (Scope -> m a) ->
  m a
scoped_ action =
  scoped action >>= \case
    Left cancelled_ -> liftIO (throwIO cancelled_)
    Right value -> pure value
{-# INLINE scoped_ #-}

-- | __Context__-aware, duration-based @threadDelay@.
--
-- /Throws/:
--
--   * Throws 'Ki.CancelToken.CancelToken' if the current __context__ is (or becomes) /cancelled/.
sleep ::
  HasContext m =>
  -- |
  Duration ->
  m ()
sleep duration = do
  context <- askContext
  timeoutSTM duration (liftIO . throwIO <$> contextCancelToken context) (pure ())

-- | Variant of 'Ki.Reader.wait' that waits for up to the given duration. This is useful for giving __threads__ some
-- time to fulfill a /cancellation/ request before killing them.
waitFor ::
  MonadIO m =>
  -- |
  Scope ->
  -- |
  Duration ->
  m ()
waitFor =
  Ki.waitFor
{-# INLINE waitFor #-}

-- | @STM@ variant of 'Ki.Reader.wait'.
waitSTM ::
  -- |
  Scope ->
  STM ()
waitSTM =
  scopeWaitSTM

--

with :: HasContext m => Scope -> m a -> m a
with scope =
  undefined -- withContext (scope'context scope)
