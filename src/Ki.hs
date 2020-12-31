module Ki
  ( -- * Scope
    Ki.Scope.Scope,
    scoped,
    wait,
    waitSTM,
    waitFor,

    -- * Creating threads
    -- $creating-threads
    Ki.Thread.Thread,

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
    Ki.Duration.Duration,
    Ki.Duration.microseconds,
    Ki.Duration.milliseconds,
    Ki.Duration.seconds,
    Ki.Timeout.timeoutSTM,
    sleep,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import qualified Ki.Context
import qualified Ki.Duration
import Ki.Prelude
import qualified Ki.Scope
import qualified Ki.Thread
import qualified Ki.Timeout

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
  Ki.Scope.Scope ->
  -- |
  m a ->
  m (Ki.Thread.Thread (Either SomeException a))
async =
  unliftedFork Ki.Thread.threadAsync
{-# SPECIALIZE async :: Ki.Scope.Scope -> IO a -> IO (Ki.Thread.Thread (Either SomeException a)) #-}

-- | Variant of 'Ki.async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask ::
  MonadUnliftIO m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  ((forall x. m x -> m x) -> m a) ->
  m (Ki.Thread.Thread (Either SomeException a))
asyncWithUnmask =
  unliftedForkWithUnmask Ki.Thread.threadAsyncWithUnmask
{-# SPECIALIZE asyncWithUnmask ::
  Ki.Scope.Scope ->
  ((forall x. IO x -> IO x) -> IO a) ->
  IO (Ki.Thread.Thread (Either SomeException a))
  #-}

-- | Wait for a __thread__ to finish.
await ::
  MonadIO m =>
  -- |
  Ki.Thread.Thread a ->
  m a
await =
  liftIO . Ki.Thread.threadAwait
{-# SPECIALIZE await :: Ki.Thread.Thread a -> IO a #-}

-- | @STM@ variant of 'Ki.await'.
awaitSTM ::
  -- |
  Ki.Thread.Thread a ->
  STM a
awaitSTM =
  Ki.Thread.thread'Await

-- | Variant of 'Ki.await' that gives up after the given duration.
awaitFor ::
  MonadIO m =>
  -- |
  Ki.Thread.Thread a ->
  -- |
  Ki.Duration.Duration ->
  m (Maybe a)
awaitFor thread duration =
  liftIO (Ki.Thread.threadAwaitFor thread duration)
{-# SPECIALIZE awaitFor :: Ki.Thread.Thread a -> Ki.Duration.Duration -> IO (Maybe a) #-}

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
  Ki.Scope.Scope ->
  -- |
  m a ->
  m (Ki.Thread.Thread a)
fork =
  unliftedFork Ki.Thread.threadFork
{-# SPECIALIZE fork :: Ki.Scope.Scope -> IO a -> IO (Ki.Thread.Thread a) #-}

-- | Variant of 'Ki.fork' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ ::
  MonadUnliftIO m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  m () ->
  m ()
fork_ =
  unliftedFork Ki.Thread.threadFork_
{-# SPECIALIZE fork_ :: Ki.Scope.Scope -> IO () -> IO () #-}

-- | Variant of 'Ki.fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask ::
  MonadUnliftIO m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  ((forall x. m x -> m x) -> m a) ->
  m (Ki.Thread.Thread a)
forkWithUnmask =
  unliftedForkWithUnmask Ki.Thread.threadForkWithUnmask
{-# SPECIALIZE forkWithUnmask :: Ki.Scope.Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Ki.Thread.Thread a) #-}

-- | Variant of 'Ki.forkWithUnmask' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ ::
  MonadUnliftIO m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  ((forall x. m x -> m x) -> m ()) ->
  m ()
forkWithUnmask_ =
  unliftedForkWithUnmask Ki.Thread.threadForkWithUnmask_
{-# SPECIALIZE forkWithUnmask_ :: Ki.Scope.Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO () #-}

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
scoped :: MonadUnliftIO m => (Ki.Scope.Scope -> m a) -> m a
scoped action =
  withRunInIO \unlift -> Ki.Scope.scopeScoped Ki.Context.globalContext \scope -> unlift (action scope)
{-# SPECIALIZE scoped :: (Ki.Scope.Scope -> IO a) -> IO a #-}

-- | Duration-based @threadDelay@.
sleep :: MonadIO m => Ki.Duration.Duration -> m ()
sleep duration =
  liftIO (threadDelay (Ki.Duration.toMicroseconds duration))
{-# SPECIALIZE sleep :: Ki.Duration.Duration -> IO () #-}

-- | Wait until all __threads__ created within a __scope__ finish.
wait :: MonadIO m => Ki.Scope.Scope -> m ()
wait =
  liftIO . Ki.Scope.scopeWait
{-# SPECIALIZE wait :: Ki.Scope.Scope -> IO () #-}

-- | Variant of 'Ki.wait' that waits for up to the given duration.
waitFor :: MonadIO m => Ki.Scope.Scope -> Ki.Duration.Duration -> m ()
waitFor scope duration =
  liftIO (Ki.Scope.scopeWaitFor scope duration)
{-# SPECIALIZE waitFor :: Ki.Scope.Scope -> Ki.Duration.Duration -> IO () #-}

-- | @STM@ variant of 'Ki.wait'.
waitSTM :: Ki.Scope.Scope -> STM ()
waitSTM =
  Ki.Scope.scopeWaitSTM

--

unliftedFork :: MonadUnliftIO m => (Ki.Scope.Scope -> IO a -> IO b) -> Ki.Scope.Scope -> m a -> m b
unliftedFork forky scope action =
  withRunInIO \unlift -> forky scope (unlift action)
{-# SPECIALIZE unliftedFork :: (Ki.Scope.Scope -> IO a -> IO b) -> Ki.Scope.Scope -> IO a -> IO b #-}

unliftedForkWithUnmask ::
  MonadUnliftIO m =>
  (Ki.Scope.Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO b) ->
  Ki.Scope.Scope ->
  ((forall x. m x -> m x) -> m a) ->
  m b
unliftedForkWithUnmask forky scope action =
  withRunInIO \unlift -> forky scope \unmask -> unlift (action (liftIO . unmask . unlift))
{-# SPECIALIZE unliftedForkWithUnmask ::
  (Ki.Scope.Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO b) ->
  Ki.Scope.Scope ->
  ((forall x. IO x -> IO x) -> IO a) ->
  IO b
  #-}
