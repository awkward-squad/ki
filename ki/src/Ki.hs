-- | `ki` is a lightweight structured concurrency library.
--
-- For a variant of this API generalized to
-- @<https://hackage.haskell.org/package/unliftio-core/docs/Control-Monad-IO-Unlift.html#t:MonadUnliftIO MonadUnliftIO>@,
-- see @<https://hackage.haskell.org/package/ki-unlifted ki-unlifted>@.
--
-- Remember to link your program with @-threaded@ to use the threaded runtime!
module Ki
  ( -- * Introduction
    -- $introduction

    -- * Core API
    Scope,
    Thread,
    scoped,
    fork,
    forkTry,
    await,
    awaitAll,

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
    awaitAll,
    fork,
    forkTry,
    forkTryWith,
    forkWith,
    forkWith_,
    fork_,
    scoped,
  )
import Ki.Internal.Thread (Thread, ThreadOptions (..), await, defaultThreadOptions)
import Ki.Internal.ThreadAffinity (ThreadAffinity (..))

-- $introduction
--
-- Structured concurrency is a paradigm of concurrent programming in which a lexical scope delimits the lifetime of each
-- thread. Threads therefore form a "call tree" hierarchy in which no child can outlive its parent.
--
-- Exceptions are propagated promptly from child to parent and vice-versa:
--
--     * If an exception is raised in a child thread, the child raises the same exception in its parent, then
--       terminates.
--
--     * If an exception is raised in a parent thread, the parent first raises an exception in all of its living
--       children, waits for them to terminate, then re-raises the original exception.
--
-- All together, this library:
--
--     * Guarantees the absence of "ghost threads" (/i.e./ threads that accidentally continue to run alongside the main
--       thread after the function that spawned them returns).
--
--     * Performs prompt, bidirectional exception propagation when an exception is raised anywhere in the call tree.
--
--     * Provides a safe and flexible API that can be used directly, or with which higher-level concurrency patterns can
--       be built on top, such as worker queues, pub-sub pipelines, and supervision trees.
--
-- For a longer introduction to structured concurrency, including an educative analogy to structured programming, please
-- read Nathaniel J. Smith's blog post,
-- <https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/ "Notes on structured concurrency, or: Go statement considered harmful">.
--
-- ==== __👉 Quick start examples__
--
-- * Perform two actions concurrently, and wait for both of them to complete.
--
--     @
--     concurrently :: IO a -> IO b -> IO (a, b)
--     concurrently action1 action2 =
--       Ki.'Ki.scoped' \\scope -> do
--         thread1 <- Ki.'Ki.fork' scope action1
--         result2 <- action2
--         result1 <- atomically (Ki.'Ki.await' thread1)
--         pure (result1, result2)
--     @
--
-- * Perform two actions concurrently, and when the first action terminates, stop executing the other.
--
--     @
--     race :: IO a -> IO a -> IO a
--     race action1 action2 =
--       Ki.'Ki.scoped' \\scope -> do
--         resultVar \<- newEmptyMVar
--         _ \<- Ki.'Ki.fork' scope (action1 \>>= tryPutMVar resultVar)
--         _ \<- Ki.'Ki.fork' scope (action2 \>>= tryPutMVar resultVar)
--         takeMVar resultVar
--     @
