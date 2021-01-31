-- | Please read "Ki.Documentation" for an overview of how to use this library.
module Ki
  ( -- * Scope
    Scope,
    scoped,
    wait,
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
    await,
    awaitSTM,
    awaitFor,

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
import Ki.Internal.Context (globalContext)
import Ki.Internal.Duration (Duration, microseconds, milliseconds, seconds, toMicroseconds)
import Ki.Internal.Prelude
import Ki.Internal.Scope (Scope, scopeScoped, scopeWait, scopeWaitFor, scopeWaitSTM)
import Ki.Internal.Thread
  ( Thread (thread'Await),
    threadAsync,
    threadAsyncWithUnmask,
    threadAwait,
    threadAwaitFor,
    threadFork,
    threadForkWithUnmask,
    threadForkWithUnmask_,
    threadFork_,
  )
import Ki.Internal.Timeout (timeoutSTM)

-- | Create a child thread within a scope.
--
-- Reference manual: "Ki.Documentation#reference_manual_async"
async ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  m a ->
  m (Thread (Either SomeException a))
async =
  threadAsync
{-# INLINE async #-}

-- | Variant of 'Ki.async' that provides the thread a function that unmasks asynchronous exceptions.
--
-- Reference manual: "Ki.Documentation#reference_manual_async"
asyncWithUnmask ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  ((forall x. m x -> m x) -> m a) ->
  m (Thread (Either SomeException a))
asyncWithUnmask =
  threadAsyncWithUnmask
{-# INLINE asyncWithUnmask #-}

-- | Wait for a __thread__ to finish.
await ::
  MonadIO m =>
  -- |
  Thread a ->
  m a
await =
  threadAwait
{-# INLINE await #-}

-- | @STM@ variant of 'Ki.await'.
awaitSTM ::
  -- |
  Thread a ->
  STM a
awaitSTM =
  thread'Await

-- | Variant of 'Ki.await' that gives up after the given duration.
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

-- | Create a child __thread__ within a __scope__.
--
-- If the child throws an exception, the exception is immediately propagated to its parent.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  m a ->
  m (Thread a)
fork =
  threadFork
{-# INLINE fork #-}

-- | Variant of 'Ki.fork' that does not return a handle to the child __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  m () ->
  m ()
fork_ =
  threadFork_
{-# INLINE fork_ #-}

-- | Variant of 'Ki.fork' that provides the child __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  ((forall x. m x -> m x) -> m a) ->
  m (Thread a)
forkWithUnmask =
  threadForkWithUnmask
{-# INLINE forkWithUnmask #-}

-- | Variant of 'Ki.forkWithUnmask' that does not return a handle to the child __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ ::
  MonadUnliftIO m =>
  -- |
  Scope ->
  -- |
  ((forall x. m x -> m x) -> m ()) ->
  m ()
forkWithUnmask_ =
  threadForkWithUnmask_
{-# INLINE forkWithUnmask_ #-}

-- | Open a __scope__, perform an action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- ==== __Examples__
--
-- @
-- 'Ki.scoped' \\scope -> do
--   'Ki.fork_' scope worker1
--   'Ki.fork_' scope worker2
--   'Ki.wait' scope
-- @
scoped ::
  MonadUnliftIO m =>
  -- |
  (Scope -> m a) ->
  m a
scoped action =
  scopeScoped globalContext action >>= \case
    Left cancelled -> liftIO (throwIO cancelled)
    Right value -> pure value
{-# INLINE scoped #-}

-- | Duration-based @threadDelay@.
sleep ::
  MonadIO m =>
  -- |
  Duration ->
  m ()
sleep duration =
  liftIO (threadDelay (toMicroseconds duration))
{-# SPECIALIZE sleep :: Duration -> IO () #-}

-- | Wait until all __threads__ created within a __scope__ finish.
wait ::
  MonadIO m =>
  -- |
  Scope ->
  m ()
wait =
  scopeWait
{-# INLINE wait #-}

-- | Variant of 'Ki.wait' that waits for up to the given duration.
waitFor ::
  MonadIO m =>
  -- |
  Scope ->
  -- |
  Duration ->
  m ()
waitFor =
  scopeWaitFor
{-# INLINE waitFor #-}

-- | @STM@ variant of 'Ki.wait'.
waitSTM ::
  -- |
  Scope ->
  STM ()
waitSTM =
  scopeWaitSTM
