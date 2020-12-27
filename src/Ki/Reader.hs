module Ki.Reader
  ( -- * Context
    Context,
    Context.globalContext,
    HasContext (..),

    -- * Scope
    Scope,
    scoped,
    wait,
    waitSTM,
    waitFor,

    -- * Spawning threads
    -- $spawning-threads
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

    -- * Soft-cancellation
    CancelToken,
    cancel,
    cancelled,
    cancelledSTM,

    -- * Miscellaneous
    Duration,
    Duration.microseconds,
    Duration.milliseconds,
    Duration.seconds,
    timeoutSTM,
    sleep,
  )
where

import Ki.CancelToken (CancelToken)
import Ki.Context (Context)
import qualified Ki.Context as Context
import Ki.Duration (Duration)
import qualified Ki.Duration as Duration
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope as Scope
import Ki.Thread (Thread)
import qualified Ki.Thread as Thread

-- $spawning-threads
--
-- There are two variants of __thread__-creating functions with different exception-propagation semantics.
--
-- * If a __thread__ created with 'Ki.Reader.fork' throws an exception, it is immediately propagated up the call tree to
-- the __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'Ki.Reader.async' throws an exception, it is not propagated up the call tree, but can
-- be observed by 'Ki.Reader.await'.

class MonadIO m => HasContext m where
  askContext :: m Context
  withContext :: Context -> m a -> m a
  withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b

-- | Create a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: HasContext m => Scope -> m a -> m (Thread (Either SomeException a))
async =
  unliftedFork Thread.async

-- | Variant of 'Ki.Reader.async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: HasContext m => Scope -> ((forall x. m x -> m x) -> m a) -> m (Thread (Either SomeException a))
asyncWithUnmask =
  unliftedForkWithUnmask Thread.asyncWithUnmask

-- | Wait for a __thread__ to finish.
await :: MonadIO m => Thread a -> m a
await =
  liftIO . Thread.await

-- | Variant of 'Ki.Reader.await' that gives up after the given duration.
awaitFor :: MonadIO m => Thread a -> Duration -> m (Maybe a)
awaitFor thread duration =
  liftIO (Thread.awaitFor thread duration)

-- | @STM@ variant of 'Ki.Reader.await'.
awaitSTM :: Thread a -> STM a
awaitSTM =
  Thread.awaitSTM

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel :: MonadIO m => Scope -> m ()
cancel =
  liftIO . Scope.cancel

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The cancel token may be thrown
-- to fulfill the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with a
-- value.
cancelled :: HasContext m => m (Maybe CancelToken)
cancelled = do
  action <- cancelledSTM
  liftIO (atomically (optional action))

-- | @STM@ variant of 'Ki.Reader.cancelled'; blocks until the current __context__ is /cancelled/.
cancelledSTM :: HasContext m => m (STM CancelToken)
cancelledSTM =
  Context.contextCancelTokenSTM <$> askContext

-- | Create a __thread__ within a __scope__.
--
-- If the __thread__ throws an exception, the exception is immediately propagated up the call tree to the __thread__
-- that opened its __scope__, unless that exception is a 'CancelToken' that fulfills a /cancellation/ request.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: HasContext m => Scope -> m a -> m (Thread a)
fork =
  unliftedFork Thread.fork

-- | Variant of 'Ki.Reader.fork' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ :: HasContext m => Scope -> m () -> m ()
fork_ =
  unliftedFork Thread.fork_

-- | Variant of 'Ki.Reader.fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: HasContext m => Scope -> ((forall x. m x -> m x) -> m a) -> m (Thread a)
forkWithUnmask =
  unliftedForkWithUnmask Thread.forkWithUnmask

-- | Variant of 'Ki.Reader.forkWithUnmask' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ :: HasContext m => Scope -> ((forall x. m x -> m x) -> m ()) -> m ()
forkWithUnmask_ =
  unliftedForkWithUnmask Thread.forkWithUnmask_

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
scoped :: HasContext m => (Scope -> m a) -> m a
scoped action = do
  context <- askContext
  withRunInIO \unlift -> Scope.scoped context \scope -> unlift (with scope (action scope))

-- | __Context__-aware, duration-based @threadDelay@.
--
-- /Throws/:
--
--   * Throws 'CancelToken' if the current __context__ is (or becomes) /cancelled/.
sleep :: HasContext m => Duration -> m ()
sleep duration = do
  context <- askContext
  (delay, unregister) <- liftIO (registerDelay (Duration.toMicroseconds duration))
  join
    ( liftIO
        ( atomically
            ( delay $> pure () <|> do
                token <- Context.contextCancelTokenSTM context
                pure (liftIO (unregister >> throwIO token))
            )
        )
    )

-- | Wait for an @STM@ action to return an @m@ action, or if the given duration elapses, return the given @m@ action
-- instead.
timeoutSTM :: MonadIO m => Duration -> STM (m a) -> m a -> m a
timeoutSTM duration action fallback = do
  -- implementation duplicated because of dejafu test suite - no MonadIO
  (delay, unregister) <- liftIO (registerDelay (Duration.toMicroseconds duration))
  join (liftIO (atomically (delay $> fallback <|> (liftIO unregister >>) <$> action)))

-- | Wait until all __threads__ created within a __scope__ finish.
wait :: MonadIO m => Scope -> m ()
wait =
  liftIO . Scope.wait

-- | @STM@ variant of 'Ki.Reader.wait'.
waitSTM :: Scope -> STM ()
waitSTM =
  Scope.waitSTM

-- | Variant of 'Ki.Reader.wait' that waits for up to the given duration. This is useful for giving __threads__ some
-- time to fulfill a /cancellation/ request before killing them.
waitFor :: MonadIO m => Scope -> Duration -> m ()
waitFor scope duration =
  liftIO (Scope.waitFor scope duration)

--

unliftedFork :: HasContext m => (Scope -> IO a -> IO b) -> Scope -> m a -> m b
unliftedFork forky scope action =
  withRunInIO \unlift -> forky scope (unlift (with scope action))

unliftedForkWithUnmask ::
  HasContext m =>
  (Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO b) ->
  Scope ->
  ((forall x. m x -> m x) -> m a) ->
  m b
unliftedForkWithUnmask forky scope action =
  withRunInIO \unlift ->
    forky scope \unmask ->
      unlift (with scope (action \oink -> liftIO (unmask (unlift oink))))

with :: HasContext m => Scope -> m a -> m a
with scope =
  withContext (Scope.context scope)
