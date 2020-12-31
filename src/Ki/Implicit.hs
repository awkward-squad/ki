{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit
  ( -- * Context
    Context,
    withGlobalContext,

    -- * Scope
    Ki.Scope.Scope,
    scoped,
    Ki.wait,
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
    Ki.await,
    awaitSTM,
    awaitFor,

    -- * Soft-cancellation
    Ki.CancelToken.CancelToken,
    cancel,
    cancelled,
    cancelledSTM,

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
import qualified Ki
import qualified Ki.CancelToken
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
-- * If a __thread__ created with 'Ki.Implicit.fork' throws an exception, it is immediately propagated up the call tree
-- to its __parent__, which is the __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'Ki.Implicit.async' throws an exception, it is not propagated to its __parent__, but
-- can be observed by 'Ki.Implicit.await'.
--
-- If a __thread__ is thrown an asynchronous exception, it is immediately propagated to its __parent__.

-- Note: keep this haddock up-to-date with Ki.Context.Context

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ created within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
type Context =
  ?context :: Ki.Context.Context

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
  (Context => m a) ->
  -- |
  m (Ki.Thread.Thread (Either SomeException a))
async =
  unliftedFork Ki.Thread.threadAsync
{-# SPECIALIZE async :: Ki.Scope.Scope -> (Context => IO a) -> IO (Ki.Thread.Thread (Either SomeException a)) #-}

-- | Variant of 'Ki.Implicit.async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask ::
  MonadUnliftIO m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  (Context => (forall x. m x -> m x) -> m a) ->
  m (Ki.Thread.Thread (Either SomeException a))
asyncWithUnmask =
  unliftedForkWithUnmask Ki.Thread.threadAsyncWithUnmask
{-# SPECIALIZE asyncWithUnmask ::
  Ki.Scope.Scope ->
  (Context => (forall x. IO x -> IO x) -> IO a) ->
  IO (Ki.Thread.Thread (Either SomeException a))
  #-}

-- | Variant of 'Ki.Implicit.await' that gives up after the given duration.
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

-- | @STM@ variant of 'Ki.Implicit.await'.
awaitSTM ::
  -- |
  Ki.Thread.Thread a ->
  STM a
awaitSTM =
  Ki.Thread.thread'Await

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel :: MonadIO m => Ki.Scope.Scope -> m ()
cancel =
  liftIO . Ki.Scope.scopeCancel
{-# SPECIALIZE cancel :: Ki.Scope.Scope -> IO () #-}

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The __cancel token__ may be
-- thrown to fulfill the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with
-- a value.
cancelled :: Context => IO (Maybe Ki.CancelToken.CancelToken)
cancelled =
  atomically (optional cancelledSTM)

-- | @STM@ variant of 'Ki.Implicit.cancelled'; blocks until the current __context__ is /cancelled/.
cancelledSTM :: Context => STM Ki.CancelToken.CancelToken
cancelledSTM =
  Ki.Context.contextCancelToken ?context

-- | Create a __thread__ within a __scope__.
--
-- If the __thread__ throws an exception, the exception is immediately propagated up the call tree to the __thread__
-- that opened its __scope__, unless the exception is a __cancel token__ that fulfills a /cancellation/ request that
-- originated in the __thread__'s __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: MonadUnliftIO m => Ki.Scope.Scope -> (Context => m a) -> m (Ki.Thread.Thread a)
fork =
  unliftedFork Ki.Thread.threadFork

-- | Variant of 'Ki.Implicit.fork' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ :: MonadUnliftIO m => Ki.Scope.Scope -> (Context => m ()) -> m ()
fork_ =
  unliftedFork Ki.Thread.threadFork_

-- | Variant of 'Ki.Implicit.fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask ::
  MonadUnliftIO m =>
  Ki.Scope.Scope ->
  (Context => (forall x. m x -> m x) -> m a) ->
  m (Ki.Thread.Thread a)
forkWithUnmask =
  unliftedForkWithUnmask Ki.Thread.threadForkWithUnmask

-- | Variant of 'Ki.Implicit.forkWithUnmask' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ ::
  MonadUnliftIO m =>
  Ki.Scope.Scope ->
  (Context => (forall x. m x -> m x) -> m ()) ->
  m ()
forkWithUnmask_ =
  unliftedForkWithUnmask Ki.Thread.threadForkWithUnmask_

-- | Perform an action in the global __context__. The global __context__ cannot be /cancelled/.
withGlobalContext :: (Context => IO a) -> IO a
withGlobalContext action =
  let ?context = Ki.Context.globalContext in action

-- | Open a __scope__, perform an action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- ==== __Examples__
--
-- @
-- 'Ki.Implicit.scoped' \\scope -> do
--   'Ki.Implicit.fork_' scope worker1
--   'Ki.Implicit.fork_' scope worker2
--   'Ki.Implicit.wait' scope
-- @
scoped :: (Context, MonadUnliftIO m) => (Context => Ki.Scope.Scope -> m a) -> m a
scoped action =
  withRunInIO \unlift -> Ki.Scope.scopeScoped ?context \scope -> unlift (with scope (action scope))
{-# SPECIALIZE scoped :: Context => (Context => Ki.Scope.Scope -> IO a) -> IO a #-}

-- | __Context__-aware, duration-based @threadDelay@.
--
-- /Throws/:
--
--   * Throws 'Ki.CancelToken.CancelToken' if the current __context__ is (or becomes) /cancelled/.
sleep :: Context => Ki.Duration.Duration -> IO ()
sleep duration =
  Ki.Timeout.timeoutSTM duration (cancelledSTM >>= throwSTM) (pure ())

-- | Variant of 'Ki.Implicit.wait' that waits for up to the given duration. This is useful for giving __threads__ some
-- time to fulfill a /cancellation/ request before killing them.
waitFor :: Ki.Scope.Scope -> Ki.Duration.Duration -> IO ()
waitFor =
  Ki.waitFor

-- | @STM@ variant of 'Ki.Implicit.wait'.
waitSTM :: Ki.Scope.Scope -> STM ()
waitSTM =
  Ki.Scope.scopeWaitSTM

--

unliftedFork ::
  MonadUnliftIO m =>
  (Ki.Scope.Scope -> IO a -> IO b) ->
  Ki.Scope.Scope ->
  (Context => m a) ->
  m b
unliftedFork forky scope action =
  withRunInIO \unlift -> forky scope (with scope (unlift action))

unliftedForkWithUnmask ::
  MonadUnliftIO m =>
  (Ki.Scope.Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO b) ->
  Ki.Scope.Scope ->
  (Context => (forall x. m x -> m x) -> m a) ->
  m b
unliftedForkWithUnmask forky scope action =
  withRunInIO \unlift -> forky scope \unmask -> unlift (with scope (action (liftIO . unmask . unlift)))

-- Ki.Thread.forkWithUnmask scope (let ?context = Ki.Scope.scope'context scope in action)

with :: Ki.Scope.Scope -> (Context => a) -> a
with scope action =
  let ?context = Ki.Scope.scope'context scope in action
