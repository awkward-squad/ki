module Ki
  ( -- * Scope
    Scope,
    scoped,
    Scope.wait,
    Scope.waitSTM,
    Scope.waitFor,

    -- * Spawning threads
    -- $spawning-threads
    Thread,

    -- ** Fork
    Thread.fork,
    Thread.fork_,
    Thread.forkWithUnmask,
    Thread.forkWithUnmask_,

    -- ** Async
    Thread.async,
    Thread.asyncWithUnmask,

    -- ** Await
    await,
    awaitSTM,
    awaitFor,

    -- * Miscellaneous
    sleep,
    Duration,
    Timeout.timeoutSTM,
    Duration.microseconds,
    Duration.milliseconds,
    Duration.seconds,

    -- * Exceptions
    ThreadFailed (..),
  )
where

import qualified Ki.Context as Context
import Ki.Duration (Duration)
import qualified Ki.Duration as Duration
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope as Scope
import Ki.Thread (Thread)
import qualified Ki.Thread as Thread
import Ki.ThreadFailed (ThreadFailed (..))
import qualified Ki.Timeout as Timeout

-- | Wait for a __thread__ to finish.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception and was created with 'Ki.Thread.fork'.
await :: Thread a -> IO a
await =
  Thread.await

-- | @STM@ variant of 'await'.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception and was created with 'Ki.Thread.fork'.
awaitSTM :: Thread a -> STM a
awaitSTM =
  Thread.awaitSTM

-- | Variant of 'await' that waits for up to the given duration.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception and was created with 'Ki.Thread.fork'.
awaitFor :: Thread a -> Duration -> IO (Maybe a)
awaitFor =
  Thread.awaitFor

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
  Scope.scoped Context.dummyContext

-- | @threadDelay@.
sleep :: Duration -> IO ()
sleep duration =
  threadDelay (Duration.toMicroseconds duration)
