{-# LANGUAGE PatternSynonyms #-}

-- | Please read "Ki.Documentation" for an overview of how to use this library.
--
-- This module exposes an API that uses an implicit parameter to pass around the __context__ implicitly. If you do not
-- intend to use soft-cancellation, you may want to use the simpler API exposed by "Ki".
module Ki.Implicit
  ( -- * Context
    Context,
    withGlobalContext,

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
    cancel,
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
import Ki.Internal.CancelToken (CancelToken)
import qualified Ki.Internal.Context
import Ki.Internal.Duration (Duration, microseconds, milliseconds, seconds)
import Ki.Internal.Prelude
import Ki.Internal.Scope (Cancelled (Cancelled), Scope (scope'context), scopeCancel, scopeScoped, scopeWaitSTM)
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

-- Note: keep this haddock up-to-date with Ki.Context.Context

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ created within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
type Context =
  ?context :: Ki.Internal.Context.Context

-- | Create a thread within a scope.
--
-- Reference manual: "Ki.Documentation#reference_manual_async"
async ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  (Context => m a) ->
  -- |
  m (Thread (Either SomeException a))
async scope action =
  threadAsync scope (with scope action)
{-# INLINE async #-}

-- | Variant of 'Ki.Implicit.async' that provides the thread a function that unmasks asynchronous exceptions.
--
-- Reference manual: "Ki.Documentation#reference_manual_async"
asyncWithUnmask ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  (Context => (forall x. m x -> m x) -> m a) ->
  m (Thread (Either SomeException a))
asyncWithUnmask scope action =
  threadAsyncWithUnmask scope \unmask -> with scope (action unmask)
{-# INLINE asyncWithUnmask #-}

-- | Variant of 'Ki.Implicit.await' that gives up after the given duration.
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

-- | @STM@ variant of 'Ki.Implicit.await'.
awaitSTM ::
  -- |
  Thread a ->
  STM a
awaitSTM =
  thread'Await

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel ::
  MonadIO m =>
  -- |
  Scope ->
  m ()
cancel =
  scopeCancel
{-# INLINE cancel #-}

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The __cancel token__ may be
-- thrown to fulfill the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with
-- a value.
cancelled ::
  (Context, MonadIO m) =>
  -- |
  m (Maybe CancelToken)
cancelled =
  liftIO (atomically (optional cancelledSTM))
{-# SPECIALIZE cancelled :: Context => IO (Maybe CancelToken) #-}

-- | @STM@ variant of 'Ki.Implicit.cancelled'; blocks until the current __context__ is /cancelled/.
cancelledSTM ::
  Context =>
  -- |
  STM CancelToken
cancelledSTM =
  Ki.Internal.Context.contextCancelToken ?context

-- | Create a child __thread__ within a __scope__.
--
-- If the child throws an exception, the exception is immediately propagated to its parent, unless the exception is a
-- __cancel token__ that originated from its parent's __scope__ being /cancelled/.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  (Context => m a) ->
  m (Thread a)
fork scope action =
  threadFork scope (with scope action)
{-# INLINE fork #-}

-- | Variant of 'Ki.Implicit.fork' that does not return a handle to the child __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  (Context => m ()) ->
  m ()
fork_ scope action =
  threadFork_ scope (with scope action)
{-# INLINE fork_ #-}

-- | Variant of 'Ki.Implicit.fork' that provides the child __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  (Context => (forall x. m x -> m x) -> m a) ->
  m (Thread a)
forkWithUnmask scope action =
  threadForkWithUnmask scope \unmask -> with scope (action unmask)
{-# INLINE forkWithUnmask #-}

-- | Variant of 'Ki.Implicit.forkWithUnmask' that does not return a handle to the child __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  (Context => (forall x. m x -> m x) -> m ()) ->
  m ()
forkWithUnmask_ scope action =
  threadForkWithUnmask_ scope \unmask -> with scope (action unmask)
{-# INLINE forkWithUnmask_ #-}

-- | Perform an action in the global __context__.
withGlobalContext :: (Context => IO a) -> IO a
withGlobalContext action =
  let ?context = Ki.Internal.Context.globalContext in action

-- | Open a __scope__, perform an action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- The __scope__'s __context__ may become /cancelled/; if it does, and the provided action fulfills the __cancellation__
-- request by throwing the corresponding __cancel token__, this function will return 'Ki.Implicit.Cancelled'.
--
-- ==== __Examples__
--
-- @
-- 'Ki.Implicit.scoped' \\scope -> do
--   'Ki.Implicit.fork_' scope worker1
--   'Ki.Implicit.fork_' scope worker2
--   'Ki.Implicit.wait' scope
-- @
scoped ::
  (Context, MonadUnliftIO m) =>
  -- |
  (Context => Scope -> m a) ->
  m (Either Cancelled a)
scoped action =
  scopeScoped ?context \scope -> with scope (action scope)
{-# INLINE scoped #-}

-- | Variant of 'Ki.Implicit.scoped' that throws 'Ki.Implicit.Cancelled' instead of returning it.
scoped_ ::
  (Context, MonadUnliftIO m) =>
  -- |
  (Context => Scope -> m a) ->
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
--   * Throws 'CancelToken' if the current __context__ is (or becomes) /cancelled/.
sleep ::
  (Context, MonadIO m) =>
  -- |
  Duration ->
  m ()
sleep duration =
  timeoutSTM duration (cancelledSTM >>= throwSTM) (pure ())
{-# SPECIALIZE sleep :: Context => Duration -> IO () #-}

-- | Variant of 'Ki.Implicit.wait' that waits for up to the given duration. This is useful for giving __threads__ some
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

-- | @STM@ variant of 'Ki.Implicit.wait'.
waitSTM ::
  -- |
  Scope ->
  STM ()
waitSTM =
  scopeWaitSTM
{-# INLINE waitSTM #-}

--

with :: Scope -> (Context => a) -> a
with scope action =
  let ?context = scope'context scope in action
