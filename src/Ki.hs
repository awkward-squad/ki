module Ki
  ( -- * Scope
    Ki.Scope.Scope,
    scoped,
    Ki.Scope.wait,
    Ki.Scope.waitSTM,
    Ki.Scope.waitFor,

    -- * Creating threads
    -- $creating-threads
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
    Ki.Thread.await,
    Ki.Thread.awaitSTM,
    Ki.Thread.awaitFor,

    -- * Miscellaneous
    Ki.Duration.Duration,
    Ki.Duration.microseconds,
    Ki.Duration.milliseconds,
    Ki.Duration.seconds,
    Ki.Timeout.timeoutSTM,
    sleep,
  )
where

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
-- * If a __thread__ created with 'Ki.fork' throws an exception, it is immediately propagated up the call tree to its
-- __parent__, which is the __thread__ that created its __scope__.
--
-- * If a __thread__ created with 'Ki.async' throws an exception, it is not propagated to its __parent__, but can be
-- observed by 'Ki.await'.
--
-- If a __thread__ is thrown an asynchronous exception, it is immediately propagated to its __parent__.

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
scoped :: (Ki.Scope.Scope -> IO a) -> IO a
scoped =
  Ki.Scope.scoped Ki.Context.globalContext

-- | Duration-based @threadDelay@.
sleep :: Ki.Duration.Duration -> IO ()
sleep duration =
  threadDelay (Ki.Duration.toMicroseconds duration)
