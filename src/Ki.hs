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
import Ki.Internal.Duration (Duration, microseconds, milliseconds, seconds)
import Ki.Internal.Prelude
import Ki.Internal.Scope
  ( Scope,
    scopeScoped,
    scopeWait,
    scopeWaitFor,
    scopeWaitSTM,
  )
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

-- | Create a child __thread__ within a __scope__.
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

-- | Variant of 'Ki.async' that provides the __thread__ a function that unmasks asynchronous exceptions.
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

-- | Wait for a __thread__ to terminate.
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
-- If the child __thread__ throws an exception, the exception is immediately propagated to its parent __thread__.
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
-- If the child throws an exception, the exception is immediately propagated to its parent, unless the exception is a
-- __cancel token__ that originated from its parent's __scope__ being /cancelled/.
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
-- The __scope__ may become /cancelled/; if it does, and the provided action fulfills the __cancellation__ request by
-- throwing the corresponding __cancel token__, this function will return 'Ki.Cancelled'.
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
scoped =
  scopeScoped
{-# INLINE scoped #-}

-- | Duration-based @threadDelay@.
sleep ::
  MonadIO m =>
  -- |
  Duration ->
  m ()
sleep duration =
  timeoutSTM duration (pure (pure ())) (pure ())
{-# SPECIALIZE sleep :: Duration -> IO () #-}

-- | Wait until all __threads__ created within a __scope__ terminate.
wait ::
  MonadIO m =>
  -- |
  Scope ->
  m ()
wait =
  scopeWait
{-# INLINE wait #-}

-- | Variant of 'Ki.wait' that waits for up to the given duration. This is useful for giving __threads__ some
-- time to fulfill a /cancellation/ request before killing them.
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
