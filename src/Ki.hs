module Ki
  ( -- * Scope
    Scope,
    scoped,
    wait,
    waitSTM,
    waitFor,

    -- * Creating threads
    -- $creating-threads
    Thread,

    -- ** Fork
    fork,
    fork_,
    forkWithUnmask,
    forkWithUnmask_,

    -- ** Async
    async,
    asyncWithUnmask,

    -- ** Await
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

-- $creating-threads
--
-- There are two variants of __thread__-creating functions with different exception-propagation semantics.
--
-- * If a __thread__ created with 'Ki.fork' throws an exception, it is immediately propagated up the call tree to its
-- __parent__, which is the __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'Ki.async' throws an exception, it is not propagated to its __parent__, but can be
-- observed by 'Ki.await'.
--
-- If a __thread__ is thrown an asynchronous exception, it is immediately propagated to its __parent__.

-- | Create a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
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
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
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

-- | Create a __thread__ within a __scope__.
--
-- If the __thread__ throws an exception, the exception is immediately propagated up the call tree to the __thread__
-- that opened its __scope__.
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

-- | Variant of 'Ki.fork' that does not return a handle to the created __thread__.
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

-- | Variant of 'Ki.fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
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

-- | Variant of 'Ki.forkWithUnmask' that does not return a handle to the created __thread__.
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
scoped =
  scopeScoped globalContext
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
