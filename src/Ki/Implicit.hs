{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit
  ( -- * Context
    Context,
    globalContext,

    -- * Scope
    Ki.Scope.Scope,
    scoped,
    Ki.Scope.wait,
    Ki.Scope.waitSTM,
    waitFor,

    -- * Spawning threads
    -- $spawning-threads
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
    Ki.Scope.cancelScope,
    cancelled,
    cancelledSTM,
    yield,
    yieldSTM,
    CancelToken,

    -- * Miscellaneous
    sleep,
    timeoutSTM,
    Duration,
    Ki.Duration.microseconds,
    Ki.Duration.milliseconds,
    Ki.Duration.seconds,

    -- * Exceptions
    ThreadFailed (..),
  )
where

import Ki.CancelToken (CancelToken)
import qualified Ki.Context
import Ki.Duration (Duration)
import qualified Ki.Duration
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope
import Ki.Thread (Thread)
import qualified Ki.Thread
import Ki.ThreadFailed (ThreadFailed (..))
import Ki.Timeout (timeoutSTM)

-- $spawning-threads
--
-- There are two variants of __thread__-creating functions with different exception-propagation semantics.
--
-- * If a __thread__ created with 'fork' throws an exception, it is immediately propagated up the call tree to the
-- __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'async' throws an exception, it is not propagated up the call tree, but can be
-- observed by 'Ki.Thread.await'.

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ created within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
type Context =
  ?context :: Ki.Context.Context

-- | Create a __thread__ within a __scope__ to compute a value concurrently.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> (Context => IO a) -> IO (Thread (Either ThreadFailed a))
async scope action =
  Ki.Thread.async scope (with scope action)

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either ThreadFailed a))
asyncWithUnmask scope action =
  Ki.Thread.asyncWithUnmask scope (let ?context = Ki.Scope.context scope in action)

-- | Wait for a __thread__ to finish.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception and was created with 'fork'.
await :: Ki.Thread.Thread a -> IO a
await =
  Ki.Thread.await

-- | @STM@ variant of 'await'.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception and was created with 'fork'.
awaitSTM :: Ki.Thread.Thread a -> STM a
awaitSTM =
  Ki.Thread.awaitSTM

-- | Variant of 'await' that waits for up to the given duration.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception and was created with 'fork'.
awaitFor :: Ki.Thread.Thread a -> Duration -> IO (Maybe a)
awaitFor =
  Ki.Thread.awaitFor

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The cancel token may be thrown
-- to fulfill the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with a
-- value.
cancelled :: Context => IO (Maybe CancelToken)
cancelled =
  atomically (optional cancelledSTM)

-- | @STM@ variant of 'cancelled'; blocks until the current __context__ is /cancelled/.
cancelledSTM :: Context => STM CancelToken
cancelledSTM =
  Ki.Context.cancelled ?context

-- | Create a __thread__ within a __scope__ to compute a value concurrently.
--
-- If the __thread__ throws an exception, the exception is wrapped in 'ThreadFailed' and immediately propagated up the
-- call tree to the __thread__ that opened its __scope__, unless that exception is a 'CancelToken' that fulfills a
-- /cancellation/ request.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: Scope -> (Context => IO a) -> IO (Thread a)
fork scope action =
  Ki.Thread.fork scope (with scope action)

-- | Variant of 'fork' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ :: Scope -> (Context => IO ()) -> IO ()
fork_ scope action =
  Ki.Thread.fork_ scope (with scope action)

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
forkWithUnmask scope action =
  Ki.Thread.forkWithUnmask scope (let ?context = Ki.Scope.context scope in action)

-- | Variant of 'forkWithUnmask' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask_ scope action =
  Ki.Thread.forkWithUnmask_ scope (let ?context = Ki.Scope.context scope in action)

-- | Perform an @IO@ action in the global __context__. The global __context__ cannot be /cancelled/.
globalContext :: (Context => IO a) -> IO a
globalContext action =
  let ?context = Ki.Context.globalContext in action

-- | Open a __scope__, perform an @IO@ action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- /Throws/:
--
--   * The exception thrown by the callback to 'scoped' itself, if any.
--   * 'ThreadFailed' containing the first exception a __thread__ created with 'fork' throws, if any.
--
-- ==== __Examples__
--
-- @
-- 'scoped' \\scope -> do
--   'fork_' scope worker1
--   'fork_' scope worker2
--   'Ki.Scope.wait' scope
-- @
scoped :: Context => (Context => Scope -> IO a) -> IO a
scoped action =
  Ki.Scope.scoped ?context \scope -> with scope (action scope)

-- | __Context__-aware @threadDelay@.
--
-- /Throws/:
--
--   * Throws 'CancelToken' if the current __context__ is /cancelled/.
sleep :: Context => Duration -> IO ()
sleep duration =
  timeoutSTM duration yieldSTM (pure ())

-- | Variant of 'Ki.Scope.wait' that waits for up to the given duration. This is useful for giving __threads__ some time
-- to fulfill a cancellation request before killing them.
waitFor :: Scope -> Duration -> IO ()
waitFor =
  Ki.Scope.waitFor

-- | Variant of 'cancelled' that throws the cancel token, if any.
--
-- /Throws/:
--
--   * Throws 'CancelToken' if the current __context__ is /cancelled/.
yield :: Context => IO ()
yield =
  atomically (yieldSTM <|> pure ())

-- | @STM@ variant of 'yield'.
--
-- /Throws/:
--
--   * Throws 'CancelToken' if the current __context__ is /cancelled/.
yieldSTM :: Context => STM a
yieldSTM =
  cancelledSTM >>= throwSTM

--

with :: Scope -> (Context => a) -> a
with scope action =
  let ?context = Ki.Scope.context scope in action
