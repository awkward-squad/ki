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
    Thread.await,
    Thread.awaitSTM,
    Thread.awaitFor,

    -- * Miscellaneous
    Duration,
    Duration.microseconds,
    Duration.milliseconds,
    Duration.seconds,
    Timeout.timeoutSTM,
    sleep,
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
import qualified Ki.Timeout as Timeout

-- $spawning-threads
--
-- There are two variants of __thread__-creating functions with different exception-propagation semantics.
--
-- * If a __thread__ created with 'Ki.fork' throws an exception, it is immediately propagated up the call tree to the
-- __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'Ki.async' throws an exception, it is not propagated up the call tree, but can be
-- observed by 'Ki.await'.

-- | Open a __scope__, perform an @IO@ action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- /Throws/:
--
--   * The exception thrown by the callback to 'scoped' itself, if any.
--   * The first exception thrown by or to a __thread__ created with 'Ki.fork', if any.
--
-- ==== __Examples__
--
-- @
-- 'scoped' \\scope -> do
--   'Ki.fork_' scope worker1
--   'Ki.fork_' scope worker2
--   'Ki.wait' scope
-- @
scoped :: (Scope -> IO a) -> IO a
scoped =
  Scope.scoped Context.dummyContext

-- | Duration-based @threadDelay@.
sleep :: Duration -> IO ()
sleep duration =
  threadDelay (Duration.toMicroseconds duration)
