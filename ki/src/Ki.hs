-- | `ki` is a lightweight concurrency library.
--
-- For a generalized variant of this API that uses @<https://hackage.haskell.org/package/unliftio-core unliftio-core>@,
-- see @<https://hackage.haskell.org/package/ki-unlifted ki-unlifted>@.
--
-- ==== Structured concurrency
--
-- * Every thread is created within a lexical scope.
-- * A thread cannot outlive the scope in which it was created.
--
-- ==== Bidirectional exception propagation
--
-- * If an unexpected exception is raised in a child thread, the exception is propagated to its parent.
-- * If an exception is raised in a parent thread, the parent raises an asynchronous exception in all of its children.
--
-- ==== __👉 Quick start example__
--
-- First, create a new lexical scope in which threads can be created.
--
-- @
-- 'scoped' \\scope ->
-- @
--
-- Next, create a couple threads.
--
-- @
--   thread1 <- 'fork' scope action1
--   thread2 <- 'fork' scope action2
-- @
--
-- Finally, wait for the threads to terminate.
--
-- @
--   result1 <- atomically ('await' thread1)
--   result2 <- atomically ('await' thread2)
--   pure (result1, result2)
-- @
module Ki
  ( -- * Core API
    Scope,
    Thread,
    scoped,
    fork,
    forktry,
    await,
    wait,

    -- * Extended API
    fork_,
    forkWith,
    forkWith_,
    forktryWith,

    -- ** Thread options
    ThreadOptions (..),
    defaultThreadOptions,
    ThreadAffinity (..),

    -- ** Byte count
    ByteCount,
    kilobytes,
    megabytes,
  )
where

import Ki.ByteCount (ByteCount, kilobytes, megabytes)
import Ki.Scope
  ( Scope,
    Thread,
    ThreadAffinity (..),
    ThreadOptions (..),
    await,
    defaultThreadOptions,
    fork,
    forkWith,
    forkWith_,
    fork_,
    forktry,
    forktryWith,
    scoped,
    wait,
  )
