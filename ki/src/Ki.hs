-- | `ki` is a lightweight concurrency library.
--
-- For a variant of this API that uses @<https://hackage.haskell.org/package/unliftio-core unliftio-core>@, see
-- @<https://hackage.haskell.org/package/ki-unlifted ki-unlifted>@.
--
-- ==== Structured concurrency
--
-- * Every thread is created within a lexical scope.
-- * A thread cannot outlive the scope in which it was created.
--
-- ==== Parent-child thread relationship
--
-- * A thread __C__'s parent __P__ is the thread that opened the lexical scope in which __C__ was created. The main
--   thread doesn't have a parent.
--
-- * A thread __P__'s children __Cs__ are the threads that were created within lexical scopes that __P__ opened.
--
-- ==== Return value
--
-- * Every thread has a return value that can be awaited.
--
-- ==== Bidirectional exception propagation
--
-- * If an unexpected exception is raised in a child thread, the exception is propagated to the parent.
--
-- * If an exception is raised in a parent, the parent kills all of its children and waits for them to terminate before
--   re-raising the exception.
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

    -- ** Thread options
    ThreadOptions (..),
    defaultThreadOptions,
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
