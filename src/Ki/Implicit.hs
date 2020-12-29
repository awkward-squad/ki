{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit
  ( -- * Context
    Context,
    withGlobalContext,

    -- * Scope
    Ki.Scope.Scope,
    scoped,
    Ki.Scope.wait,
    Ki.Scope.waitSTM,
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
    Ki.Thread.await,
    Ki.Thread.awaitSTM,
    Ki.Thread.awaitFor,

    -- * Soft-cancellation
    Ki.CancelToken.CancelToken,
    Ki.Scope.cancel,
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
async :: Ki.Scope.Scope -> (Context => IO a) -> IO (Ki.Thread.Thread (Either SomeException a))
async scope action =
  Ki.Thread.async scope (with scope action)

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask ::
  Ki.Scope.Scope ->
  (Context => (forall x. IO x -> IO x) -> IO a) ->
  IO (Ki.Thread.Thread (Either SomeException a))
asyncWithUnmask scope action =
  Ki.Thread.asyncWithUnmask scope (let ?context = Ki.Scope.scope'context scope in action)

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The __cancel token__ may be
-- thrown to fulfill the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with
-- a value.
cancelled :: Context => IO (Maybe Ki.CancelToken.CancelToken)
cancelled =
  atomically (optional cancelledSTM)

-- | @STM@ variant of 'cancelled'; blocks until the current __context__ is /cancelled/.
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
fork :: Ki.Scope.Scope -> (Context => IO a) -> IO (Ki.Thread.Thread a)
fork scope action =
  Ki.Thread.fork scope (with scope action)

-- | Variant of 'fork' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ :: Ki.Scope.Scope -> (Context => IO ()) -> IO ()
fork_ scope action =
  Ki.Thread.fork_ scope (with scope action)

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Ki.Scope.Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Ki.Thread.Thread a)
forkWithUnmask scope action =
  Ki.Thread.forkWithUnmask scope (let ?context = Ki.Scope.scope'context scope in action)

-- | Variant of 'forkWithUnmask' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ :: Ki.Scope.Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask_ scope action =
  Ki.Thread.forkWithUnmask_ scope (let ?context = Ki.Scope.scope'context scope in action)

-- | Perform an @IO@ action in the global __context__. The global __context__ cannot be /cancelled/.
withGlobalContext :: (Context => IO a) -> IO a
withGlobalContext action =
  let ?context = Ki.Context.globalContext in action

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
scoped :: Context => (Context => Ki.Scope.Scope -> IO a) -> IO a
scoped action =
  Ki.Scope.scoped ?context \scope -> with scope (action scope)

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
  Ki.Scope.waitFor

--

with :: Ki.Scope.Scope -> (Context => a) -> a
with scope action =
  let ?context = Ki.Scope.scope'context scope in action
