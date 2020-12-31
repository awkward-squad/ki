module Ki.Reader
  ( -- * Context
    Ki.Context.Context,
    Ki.Context.globalContext,
    HasContext (..),

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
    Ki.Implicit.cancel,
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

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Ki
import qualified Ki.CancelToken
import qualified Ki.Context
import qualified Ki.Duration
import qualified Ki.Implicit
import Ki.Prelude
import qualified Ki.Scope
import qualified Ki.Thread
import qualified Ki.Timeout

-- $creating-threads
--
-- There are two variants of __thread__-creating functions with different exception-propagation semantics.
--
-- * If a __thread__ created with 'Ki.Reader.fork' throws an exception, it is immediately propagated up the call tree to
-- its __parent__, which is the __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'Ki.Reader.async' throws an exception, it is not propagated to its __parent__, but can
-- be observed by 'Ki.Reader.await'.
--
-- If a __thread__ is thrown an asynchronous exception, it is immediately propagated to its __parent__.

class MonadUnliftIO m => HasContext m where
  askContext :: m Ki.Context.Context
  withContext :: Ki.Context.Context -> m a -> m a

-- | Create a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async ::
  HasContext m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  m a ->
  m (Ki.Thread.Thread (Either SomeException a))
async scope action =
  Ki.Thread.threadAsync scope (with scope action)

-- | Variant of 'Ki.Reader.async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask ::
  HasContext m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  ((forall x. m x -> m x) -> m a) ->
  m (Ki.Thread.Thread (Either SomeException a))
asyncWithUnmask scope action =
  Ki.Thread.threadAsyncWithUnmask scope \unmask -> with scope (action unmask)

-- | Variant of 'Ki.Reader.await' that gives up after the given duration.
awaitFor ::
  MonadIO m =>
  -- |
  Ki.Thread.Thread a ->
  -- |
  Ki.Duration.Duration ->
  m (Maybe a)
awaitFor =
  Ki.Thread.threadAwaitFor
{-# INLINE awaitFor #-}

-- | @STM@ variant of 'Ki.Reader.await'.
awaitSTM ::
  -- |
  Ki.Thread.Thread a ->
  STM a
awaitSTM =
  Ki.Thread.thread'Await

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The __cancel token__ may be
-- thrown to fulfill the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with
-- a value.
cancelled ::
  HasContext m =>
  -- |
  m (Maybe Ki.CancelToken.CancelToken)
cancelled = do
  action <- cancelledSTM
  liftIO (atomically (optional action))

-- | @STM@ variant of 'Ki.Reader.cancelled'; blocks until the current __context__ is /cancelled/.
cancelledSTM ::
  HasContext m =>
  -- |
  m (STM Ki.CancelToken.CancelToken)
cancelledSTM =
  Ki.Context.contextCancelToken <$> askContext

-- | Create a __thread__ within a __scope__.
--
-- If the __thread__ throws an exception, the exception is immediately propagated up the call tree to the __thread__
-- that opened its __scope__, unless the exception is a __cancel token__ that fulfills a /cancellation/ request that
-- originated in the __thread__'s __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork ::
  HasContext m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  m a ->
  m (Ki.Thread.Thread a)
fork scope action =
  Ki.Thread.threadFork scope (with scope action)

-- | Variant of 'Ki.Reader.fork' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ ::
  HasContext m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  m () ->
  m ()
fork_ scope action =
  Ki.Thread.threadFork_ scope (with scope action)

-- | Variant of 'Ki.Reader.fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask ::
  HasContext m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  ((forall x. m x -> m x) -> m a) ->
  m (Ki.Thread.Thread a)
forkWithUnmask scope action =
  Ki.Thread.threadForkWithUnmask scope \unmask -> with scope (action unmask)

-- | Variant of 'Ki.Reader.forkWithUnmask' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ ::
  HasContext m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  ((forall x. m x -> m x) -> m ()) ->
  m ()
forkWithUnmask_ scope action =
  Ki.Thread.threadForkWithUnmask_ scope \unmask -> with scope (action unmask)

-- | Open a __scope__, perform an action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
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
  (Ki.Scope.Scope -> m a) ->
  -- |
  m a
scoped action = do
  context <- askContext
  Ki.Scope.scopeScoped context \scope -> with scope (action scope)

-- | __Context__-aware, duration-based @threadDelay@.
--
-- /Throws/:
--
--   * Throws 'Ki.CancelToken.CancelToken' if the current __context__ is (or becomes) /cancelled/.
sleep ::
  HasContext m =>
  -- |
  Ki.Duration.Duration ->
  m ()
sleep duration = do
  context <- askContext
  Ki.Timeout.timeoutSTM duration (liftIO . throwIO <$> Ki.Context.contextCancelToken context) (pure ())

-- | Variant of 'Ki.Reader.wait' that waits for up to the given duration. This is useful for giving __threads__ some
-- time to fulfill a /cancellation/ request before killing them.
waitFor ::
  MonadIO m =>
  -- |
  Ki.Scope.Scope ->
  -- |
  Ki.Duration.Duration ->
  m ()
waitFor =
  Ki.waitFor
{-# INLINE waitFor #-}

-- | @STM@ variant of 'Ki.Reader.wait'.
waitSTM ::
  -- |
  Ki.Scope.Scope ->
  STM ()
waitSTM =
  Ki.Scope.scopeWaitSTM

--

with :: HasContext m => Ki.Scope.Scope -> m a -> m a
with scope =
  withContext (Ki.Scope.scope'context scope)
