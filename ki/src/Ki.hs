-- | `ki` is a lightweight concurrency library with a few distinctive features.
--
-- For a variant of this API that uses @<https://hackage.haskell.org/package/unliftio-core unliftio-core>@, see
-- @<https://hackage.haskell.org/package/ki-unlifted ki-unlifted>@.
--
-- ==== Structured concurrency
--
-- * Every thread is created within a lexical scope.
-- * A thread cannot outlive the scope in which it was created.
--
-- Thus, whether any given function internally creates threads is an
-- implementation detail. By the time the function returns, any threads it may
-- have created are guaranteed to have terminated.
--
-- ==== Parent-child thread relationship
--
-- * A thread's parent is the thread that opened the lexical scope in which the
--   thread was created. The main thread doesn't have a parent.
--
-- * A thread's children are all of the threads that were created within lexical
--   scopes that the thread opened.
--
-- Threads therefore form a "call tree", in which the main thread is the root
-- node, and each node has zero or more children.
--
-- ==== Threads compute values
--
-- * Every thread has a return value that can be awaited.
--
-- For example, a thread that computes the 45th fibonacci number might have a
-- return value of type 'Integer', whereas a thread that never returns might
-- have a return value of type 'Data.Void.Void'.
--
-- ==== Threads may throw exceptions, which may be expected
--
-- * A thread may fail to compute its value, and instead throw an exception,
--   which may considered by the programmer to be expected or unexpected.
--
-- For example, a thread that downloads a webpage may be expected to sometimes
-- throw a @NetworkDown@ exception, but never a @DatabaseCorrupt@ exception.
--
-- ==== Bidirectional exception propagation
--
-- * If a child thread throws (or is thrown) an unexpected exception, it
--   propagates the exception to its parent before terminating.
--
-- * If a parent thread throws (or is thrown) an exception, it kills all of its
--   children and waits for them to terminate before propagating the exception
--   up its call stack.
module Ki
  ( -- * Core API
    Scope,
    scoped,
    wait,
    Thread,
    fork,
    forktry,
    await,

    -- * Extended API
    fork_,
    forkWith,
    forkWith_,
    forktryWith,
    awaitSTM,

    -- ** Thread options
    ThreadOpts (..),
    defaultThreadOpts,
    ThreadAffinity (..),

    -- ** Bytes
    Bytes,
    kilobytes,
    megabytes,
  )
where

import Ki.Bytes (Bytes, kilobytes, megabytes)
import Ki.Scope
  ( Scope,
    Thread,
    ThreadAffinity (..),
    ThreadOpts (..),
    await,
    awaitSTM,
    defaultThreadOpts,
    fork,
    forkWith,
    forkWith_,
    fork_,
    forktry,
    forktryWith,
    scoped,
    wait,
  )
