module Ki
  ( -- * Scope
    scoped,
    Ki.Scope.wait,
    Ki.Scope.waitSTM,
    Ki.Scope.waitFor,
    Scope,

    -- * Spawning threads
    -- $spawning-threads
    Ki.Thread.fork,
    Ki.Thread.fork_,
    Ki.Thread.forkWithUnmask,
    Ki.Thread.forkWithUnmask_,
    Ki.Thread.async,
    Ki.Thread.asyncWithUnmask,
    Ki.Thread.await,
    Ki.Thread.awaitSTM,
    Ki.Thread.awaitFor,
    Ki.Thread.Thread,

    -- * Miscellaneous
    sleep,
    Ki.Timeout.timeoutSTM,
    Duration,
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

-- $spawning-threads
--
-- There are two kinds of __thread__ with different exception-propagation semantics.
--
-- * If a __thread__ created with 'fork' throws an exception, it is immediately propagated up the call tree to the
-- __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'async' throws an exception, it is not propagated up the call tree, but can be
-- observed by 'Ki.Thread.await'.

-- | Open a __scope__, perform an @IO@ action with it, then close it.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- It is generally not advised to pass a __scope__ into a function, or share it amongst __threads__, as this takes the
-- "structure" out of "structured concurrency".
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
