{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit
  ( -- * Context
    Context,
    withGlobalContext,

    -- * Scope
    Scope,
    scoped,
    Scope.wait,
    Scope.waitSTM,
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
    Thread.await,
    Thread.awaitSTM,
    Thread.awaitFor,

    -- * Soft-cancellation
    CancelToken,
    Scope.cancel,
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
import qualified Ki.Context as Context
import Ki.Duration (Duration)
import qualified Ki.Duration as Duration
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope as Scope
import Ki.Thread (Thread)
import qualified Ki.Thread as Thread
import Ki.Timeout (timeoutSTM)

-- $spawning-threads
--
-- There are two variants of __thread__-creating functions with different exception-propagation semantics.
--
-- * If a __thread__ created with 'fork' throws an exception, it is immediately propagated up the call tree to the
-- __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'async' throws an exception, it is not propagated up the call tree, but can be
-- observed by 'Ki.Implicit.await'.

-- Note: keep this haddock up-to-date with Ki.Context.Context

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ created within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
type Context =
  ?context :: Context.Context

-- | Create a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> (Context => IO a) -> IO (Thread (Either SomeException a))
async scope action =
  Thread.async scope (with scope action)

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either SomeException a))
asyncWithUnmask scope action =
  Thread.asyncWithUnmask scope (let ?context = Scope.context scope in action)

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
  Context.contextCancelTokenSTM ?context

-- | Create a __thread__ within a __scope__.
--
-- If the __thread__ throws an exception, the exception is immediately propagated up the call tree to the __thread__
-- that opened its __scope__, unless that exception is a 'CancelToken' that fulfills a /cancellation/ request.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: Scope -> (Context => IO a) -> IO (Thread a)
fork scope action =
  Thread.fork scope (with scope action)

-- | Variant of 'fork' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ :: Scope -> (Context => IO ()) -> IO ()
fork_ scope action =
  Thread.fork_ scope (with scope action)

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
forkWithUnmask scope action =
  Thread.forkWithUnmask scope (let ?context = Scope.context scope in action)

-- | Variant of 'forkWithUnmask' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask_ scope action =
  Thread.forkWithUnmask_ scope (let ?context = Scope.context scope in action)

-- | Perform an @IO@ action in the global __context__. The global __context__ cannot be /cancelled/.
withGlobalContext :: (Context => IO a) -> IO a
withGlobalContext action =
  let ?context = Context.globalContext in action

-- | Open a __scope__, perform an @IO@ action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- /Throws/:
--
--   * The exception thrown by the callback to 'scoped' itself, if any.
--   * The first exception thrown by or to a __thread__ created with 'fork', if any.
--
-- ==== __Examples__
--
-- @
-- 'scoped' \\scope -> do
--   'fork_' scope worker1
--   'fork_' scope worker2
--   'Ki.Implicit.wait' scope
-- @
scoped :: Context => (Context => Scope -> IO a) -> IO a
scoped action =
  Scope.scoped ?context \scope -> with scope (action scope)

-- | __Context__-aware, duration-based @threadDelay@.
--
-- /Throws/:
--
--   * Throws 'CancelToken' if the current __context__ is (or becomes) /cancelled/.
sleep :: Context => Duration -> IO ()
sleep duration =
  timeoutSTM duration (cancelledSTM >>= throwSTM) (pure ())

-- | Variant of 'Ki.Implicit.wait' that waits for up to the given duration. This is useful for giving __threads__ some
-- time to fulfill a /cancellation/ request before killing them.
waitFor :: Scope -> Duration -> IO ()
waitFor =
  Scope.waitFor

--

with :: Scope -> (Context => a) -> a
with scope action =
  let ?context = Scope.context scope in action
