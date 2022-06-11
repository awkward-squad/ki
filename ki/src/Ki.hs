-- | `ki` is a lightweight concurrency library.
--
-- For a generalized variant of this API that uses @<https://hackage.haskell.org/package/unliftio-core unliftio-core>@,
-- see @<https://hackage.haskell.org/package/ki-unlifted ki-unlifted>@.
--
-- ==== Structured concurrency
--
-- * A lexical scope delimits the lifetime of each thread.
--
--     * After a scope closes, all threads created within it are guaranteed to have terminated.
--     * A child thread, therefore, cannout outlive its parent.
--
-- ==== Bidirectional exception propagation
--
-- * If an exception is raised in a child thread, the child thread propagates the exception to its parent.
-- * If an exception is raised in a parent thread, the parent thread raises an exception in all of its children.
--
-- ==== __👉 Quick start examples__
--
-- * Perform two actions concurrently, and wait for both of them to complete.
--
--     @
--     'Ki.scoped' \\scope -> do
--       thread1 <- 'Ki.fork' scope action1
--       thread2 <- 'Ki.fork' scope action2
--       result1 <- atomically ('Ki.await' thread1)
--       result2 <- atomically ('Ki.await' result2)
--       pure (result1, result2)
--     @
--
-- * Perform two actions concurrently, and when @action1@ completes, stop executing @action2@.
--
--     @
--     'Ki.scoped' \\scope -> do
--       'Ki.fork_' scope action2
--       action1
--     @
--
-- * Perform two actions concurrently, and when the first one finishes, stop executing the other.
--
--     @
--     'Ki.scoped' \\scope -> do
--       resultVar \<- newEmptyMVar
--       _ \<- 'Ki.fork' scope (action1 \>>= tryPutMVar resultVar)
--       _ \<- 'Ki.fork' scope (action2 \>>= tryPutMVar resultVar)
--       takeMVar resultVar
--     @
module Ki
  ( -- * Core API
    Scope,
    Thread,
    scoped,
    fork,
    forkTry,
    await,
    wait,

    -- * Extended API
    fork_,
    forkWith,
    forkWith_,
    forkTryWith,

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

import Ki.Internal.ByteCount (ByteCount, kilobytes, megabytes)
import Ki.Internal.Scope
  ( Scope,
    fork,
    forkWith,
    forkWith_,
    fork_,
    forkTry,
    forkTryWith,
    scoped,
    wait,
  )
import Ki.Internal.Thread
  ( Thread,
    ThreadAffinity (..),
    ThreadOptions (..),
    await,
    defaultThreadOptions,
  )
