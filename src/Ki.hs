module Ki
  ( -- * Scope
    Scope,
    scoped,
    Ki.Scope.wait,
    Ki.Scope.waitSTM,
    Ki.Scope.waitFor,

    -- * Spawning threads
    -- $spawning-threads
    Ki.Thread.Thread,

    -- ** Fork
    Ki.Thread.fork,
    Ki.Thread.fork_,
    Ki.Thread.forkWithUnmask,
    Ki.Thread.forkWithUnmask_,

    -- ** Async
    Ki.Thread.async,
    Ki.Thread.asyncWithUnmask,

    -- ** Await
    await,
    awaitSTM,
    awaitFor,

    -- * Miscellaneous
    sleep,
    Duration,
    Ki.Timeout.timeoutSTM,
    Ki.Duration.microseconds,
    Ki.Duration.milliseconds,
    Ki.Duration.seconds,

    -- * Exceptions
    ThreadFailed (..),
  )
where

import qualified Ki.Context
import Ki.Duration (Duration)
import qualified Ki.Duration
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope
import qualified Ki.Thread
import Ki.ThreadFailed (ThreadFailed (..))
import qualified Ki.Timeout

-- | Wait for a __thread__ to finish.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception and was created with 'Ki.Thread.fork'.
await :: Ki.Thread.Thread a -> IO a
await =
  Ki.Thread.await

-- | @STM@ variant of 'await'.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception and was created with 'Ki.Thread.fork'.
awaitSTM :: Ki.Thread.Thread a -> STM a
awaitSTM =
  Ki.Thread.awaitSTM

-- | Variant of 'await' that waits for up to the given duration.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception and was created with 'Ki.Thread.fork'.
awaitFor :: Ki.Thread.Thread a -> Duration -> IO (Maybe a)
awaitFor =
  Ki.Thread.awaitFor

-- $spawning-threads
--
-- There are two variants of __thread__-creating functions with different exception-propagation semantics.
--
-- * If a __thread__ created with 'Ki.Thread.fork' throws an exception, it is immediately propagated up the call tree to
-- the __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'Ki.Thread.async' throws an exception, it is not propagated up the call tree, but can
-- be observed by 'Ki.Thread.await'.

-- | Open a __scope__, perform an @IO@ action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- /Throws/:
--
--   * The exception thrown by the callback to 'scoped' itself, if any.
--   * 'ThreadFailed' containing the first exception a __thread__ created with 'Ki.Thread.fork' throws, if any.
--
-- ==== __Examples__
--
-- @
-- 'scoped' \\scope -> do
--   'Ki.Thread.fork_' scope worker1
--   'Ki.Thread.fork_' scope worker2
--   'Ki.Scope.wait' scope
-- @
scoped :: (Scope -> IO a) -> IO a
scoped =
  Ki.Scope.scoped Ki.Context.dummyContext

-- | @threadDelay@.
sleep :: Duration -> IO ()
sleep duration =
  threadDelay (Ki.Duration.toMicroseconds duration)
