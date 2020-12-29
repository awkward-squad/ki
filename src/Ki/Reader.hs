module Ki.Reader
  ( -- * Context
    Ki.Context.Context,
    Ki.Context.globalContext,
    HasContext (..),

    -- * Scope
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
    timeoutSTM,
    sleep,
  )
where

import qualified Ki.CancelToken
import qualified Ki.Context
import qualified Ki.Duration
import Ki.Prelude
import qualified Ki.Scope
import qualified Ki.Thread

-- $creating-threads
--
-- There are two variants of __thread__-creating functions with different exception-propagation semantics.
--
-- * If a __thread__ created with 'Ki.Reader.fork' throws an exception, it is immediately propagated up the call tree to
-- its __parent__, which is the __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'Ki.Reader.async' throws an exception, it is not propagated to its __parent__, but can
-- be observed by 'Ki.Reader.await'.

class MonadIO m => HasContext m where
  askContext :: m Ki.Context.Context
  withContext :: Ki.Context.Context -> m a -> m a
  withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b

-- | Create a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: HasContext m => Ki.Scope.Scope -> m a -> m (Ki.Thread.Thread (Either SomeException a))
async =
  unliftedFork Ki.Thread.async

-- | Variant of 'Ki.Reader.async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask ::
  HasContext m =>
  Ki.Scope.Scope ->
  ((forall x. m x -> m x) -> m a) ->
  m (Ki.Thread.Thread (Either SomeException a))
asyncWithUnmask =
  unliftedForkWithUnmask Ki.Thread.asyncWithUnmask

-- | Wait for a __thread__ to finish.
await :: MonadIO m => Ki.Thread.Thread a -> m a
await =
  liftIO . Ki.Thread.await

-- | Variant of 'Ki.Reader.await' that gives up after the given duration.
awaitFor :: MonadIO m => Ki.Thread.Thread a -> Ki.Duration.Duration -> m (Maybe a)
awaitFor thread duration =
  liftIO (Ki.Thread.awaitFor thread duration)

-- | @STM@ variant of 'Ki.Reader.await'.
awaitSTM :: Ki.Thread.Thread a -> STM a
awaitSTM =
  Ki.Thread.awaitSTM

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel :: MonadIO m => Ki.Scope.Scope -> m ()
cancel =
  liftIO . Ki.Scope.cancel

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The __cancel token__ may be
-- thrown to fulfill the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with
-- a value.
cancelled :: HasContext m => m (Maybe Ki.CancelToken.CancelToken)
cancelled = do
  action <- cancelledSTM
  liftIO (atomically (optional action))

-- | @STM@ variant of 'Ki.Reader.cancelled'; blocks until the current __context__ is /cancelled/.
cancelledSTM :: HasContext m => m (STM Ki.CancelToken.CancelToken)
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
fork :: HasContext m => Ki.Scope.Scope -> m a -> m (Ki.Thread.Thread a)
fork =
  unliftedFork Ki.Thread.fork

-- | Variant of 'Ki.Reader.fork' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ :: HasContext m => Ki.Scope.Scope -> m () -> m ()
fork_ =
  unliftedFork Ki.Thread.fork_

-- | Variant of 'Ki.Reader.fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: HasContext m => Ki.Scope.Scope -> ((forall x. m x -> m x) -> m a) -> m (Ki.Thread.Thread a)
forkWithUnmask =
  unliftedForkWithUnmask Ki.Thread.forkWithUnmask

-- | Variant of 'Ki.Reader.forkWithUnmask' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ :: HasContext m => Ki.Scope.Scope -> ((forall x. m x -> m x) -> m ()) -> m ()
forkWithUnmask_ =
  unliftedForkWithUnmask Ki.Thread.forkWithUnmask_

-- | Open a __scope__, perform an @IO@ action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- /Throws/:
--
--   * The exception thrown by the callback to 'Ki.Reader.scoped' itself, if any.
--   * The first exception thrown by or to a __thread__ created with 'Ki.Reader.fork', if any.
--
-- ==== __Examples__
--
-- @
-- 'Ki.Reader.scoped' \\scope -> do
--   'Ki.Reader.fork_' scope worker1
--   'Ki.Reader.fork_' scope worker2
--   'Ki.Reader.wait' scope
-- @
scoped :: HasContext m => (Ki.Scope.Scope -> m a) -> m a
scoped action = do
  context <- askContext
  withRunInIO \unlift -> Ki.Scope.scoped context \scope -> unlift (with scope (action scope))

-- | __Context__-aware, duration-based @threadDelay@.
--
-- /Throws/:
--
--   * Throws 'Ki.CancelToken.CancelToken' if the current __context__ is (or becomes) /cancelled/.
sleep :: HasContext m => Ki.Duration.Duration -> m ()
sleep duration = do
  context <- askContext
  timeoutSTM duration (liftIO . throwIO <$> Ki.Context.contextCancelToken context) (pure ())

-- | Wait for an @STM@ action to return an @m@ action, or if the given duration elapses, return the given @m@ action
-- instead.
timeoutSTM :: MonadIO m => Ki.Duration.Duration -> STM (m a) -> m a -> m a
timeoutSTM duration action fallback = do
  -- implementation duplicated because of dejafu test suite - no MonadIO
  (delay, unregister) <- liftIO (registerDelay (Ki.Duration.toMicroseconds duration))
  join (liftIO (atomically (delay $> fallback <|> (liftIO unregister >>) <$> action)))

-- | Wait until all __threads__ created within a __scope__ finish.
wait :: MonadIO m => Ki.Scope.Scope -> m ()
wait =
  liftIO . Ki.Scope.wait

-- | @STM@ variant of 'Ki.Reader.wait'.
waitSTM :: Ki.Scope.Scope -> STM ()
waitSTM =
  Ki.Scope.waitSTM

-- | Variant of 'Ki.Reader.wait' that waits for up to the given duration. This is useful for giving __threads__ some
-- time to fulfill a /cancellation/ request before killing them.
waitFor :: MonadIO m => Ki.Scope.Scope -> Ki.Duration.Duration -> m ()
waitFor scope duration =
  liftIO (Ki.Scope.waitFor scope duration)

--

unliftedFork :: HasContext m => (Ki.Scope.Scope -> IO a -> IO b) -> Ki.Scope.Scope -> m a -> m b
unliftedFork forky scope action =
  withRunInIO \unlift -> forky scope (unlift (with scope action))

unliftedForkWithUnmask ::
  HasContext m =>
  (Ki.Scope.Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO b) ->
  Ki.Scope.Scope ->
  ((forall x. m x -> m x) -> m a) ->
  m b
unliftedForkWithUnmask forky scope action =
  withRunInIO \unlift ->
    forky scope \unmask ->
      unlift (with scope (action \oink -> liftIO (unmask (unlift oink))))

with :: HasContext m => Ki.Scope.Scope -> m a -> m a
with scope =
  withContext (Ki.Scope.scope'context scope)
