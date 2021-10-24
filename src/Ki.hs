-- | Please read the documentation at the bottom of this module for an overview of how to use this library.
module Ki
  ( -- * Scope
    Scope,
    scoped,
    wait,
    waitSTM,
    waitFor,

    -- * Thread
    Thread,
    fork,
    fork_,
    forkWith,
    forkWith_,
    async,
    asyncWith,
    await,
    awaitSTM,
    awaitFor,

    -- ** Thread options
    ThreadOpts (..),
    defaultThreadOpts,
    ThreadAffinity (..),

    -- * Miscellaneous
    timeoutSTM,
    sleep,

    -- ** Bytes
    Bytes,
    kilobytes,
    megabytes,

    -- ** Duration
    Duration,
    microseconds,
    milliseconds,
    seconds,
    minutes,
    hours,

    -- * Documentation

    -- ** Structured concurrency
    -- $tutorial-structured-concurrency

    -- ** Creating threads
    -- $tutorial-creating-threads

    -- ** Exception propagation
    -- $tutorial-exception-propagation
  )
where

import Ki.Bytes (Bytes, kilobytes, megabytes)
import Ki.Duration (Duration, hours, microseconds, milliseconds, minutes, seconds)
import Ki.Prelude
import Ki.Scope
  ( Scope,
    Thread,
    ThreadAffinity (..),
    ThreadOpts (..),
    async,
    asyncWith,
    await,
    awaitFor,
    awaitSTM,
    defaultThreadOpts,
    fork,
    forkWith,
    forkWith_,
    fork_,
    scoped,
    wait,
    waitFor,
    waitSTM,
  )
import Ki.Timeout (timeoutSTM)

-- $documentation

-- $tutorial-structured-concurrency
--
-- "Structured concurrency" is a style of concurrent programming in which threads are always created within a lexical
-- scope, and cannot outlive the scope in which they were created.
--
-- In GHC Haskell, the lifetime of a (child) thread created with 'Control.Concurrent.forkIO' is unrelated to the
-- lifetime of the (parent) thread that created it: if a child thread terminates, its parent can continue running, and
-- vice-versa.
--
-- Although sometimes convenient, this style of concurrent programming often leads to undesirable complexity, as any IO
-- action can create a thread as a side effect, which may be left running in the background, often unintentionally,
-- after the IO action itself completes or throws an exception.
--
-- A backgrounded thread belies the humble function abstraction: if a function may create a thread that is left running
-- after the function itself returns, then the function can be considered to have multiple exit points - one additional
-- exit point for each thread left running. This is harmful for the same reasons @goto@ is considered harmful. A program
-- becomes significantly easier to understand if, whenever a function is invoked, the reader can be certain that no
-- matter what the function does internally, control will return to the very next statement.

-- $tutorial-creating-threads
--
-- In @ki@, the scope in which threads are created is a first-class value called 'Scope'. A scope can only be created
-- by the 'Ki.scoped' function, which is a "with-style" (or "bracket-style") function that accepts a callback; the
-- scope is only valid for the duration of the callback.
--
-- A thread can be only created within a scope, because all variants of creating a thread, such as 'Ki.fork' and
-- 'Ki.async' accept a 'Scope' as an explicit argument. Each (child) thread created within a scope is said to be a
-- sibling of the others, and is related to its siblings only implicitly via the relationship each has with its parent
-- (the thread that created the scope itself).
--
-- When the callback provided to 'Ki.scoped' returns, the scope becomes "closed", and no new threads can be created
-- within it. All living threads that were created within the scope are thrown an asynchronous exception, and
-- 'Ki.scoped' does not return until all of them have terminated. This satisfies the basic requirement of structured
-- concurrency: a thread cannot outlive the scope in which it was created.
--
-- Here's a simple example, annotated below.
--
-- @
-- __-- (1)__
-- (result1, result2) <-
--   Ki.'Ki.scoped' \\scope ->
--     __-- (2)__
--     thread1 <- Ki.'Ki.async' scope child1
--     thread2 <- Ki.'Ki.async' scope child2
--     __-- (3)__
--     result1 <- Ki.'Ki.await' thread1
--     result2 <- Ki.'Ki.await' thread2
--     pure (result1, result2)
--     __-- (4)__
-- __-- (5)__
-- @
--
-- 1. First, we open a new scope with 'Ki.scoped'. It's only "open" for the duration of the callback we provide.
-- 2. Next, we create two threads within the scope.
-- 3. Next, we wait for both threads to return with either a value or an exception.
-- 4. Finally, we reach the end of the callback. Here, the scope is "closed", and all living threads that were created
--    within it are thrown an asynchronous exception.
-- 5. Here, all threads that were created within it scope are guaranteed to have terminated.
--
-- An explicit scope is a powerful abstraction: although it is indeed an additional argument to pass around as compared
-- to simpler thread creation APIs such as 'Control.Concurrent.forkIO' and
-- @<https://hackage.haskell.org/package/async/docs/Control-Concurrent-Async.html#v:async async>@, the threads created
-- within it are all related to each other, and this relationship often be read off the page.
--
-- In the example above, we can see that that @child1@ and @child2@ are the only threads created within the scope; there
-- are no additional lifetimes to consider when attempting to understand the behavior of this program.
--
-- This would not be the case if the scope was explicitly passed down into @child1@, for example, which would bestow
-- @child1@ with the ability to create its own siblings, nor would it be the case if the scope was implicitly passed
-- around, as by a reader monad or similar.
--
-- Passing a scope value around is still an option, and is necessary for certain advanced use cases, as well as
-- implementing concurrency abstractions such as worker pools, actors, and supervisors. But be careful - wherever the
-- scope goes, so goes the ability to create threads within it!

-- $tutorial-exception-propagation
--
-- In @ki@, exception propagation is bi-directional between parent and child threads. We've already discussed one
-- circumstance in which a parent throws exceptions to its children: when the callback provided to 'Ki.scoped' returns.
-- But this is also the case if the parent terminates abnormally by throwing an exception, or if it is thrown an
-- asynchronous exception from another thread. No matter what happens to a parent thread with an open scope, the scope
-- will be closed, at which point all living child threads are terminated.
--
-- @
-- 'Ki.scoped' \\scope ->
--   __-- (1)__
-- __-- (2)__
-- @
--
-- 1. It does not matter how many threads are created within here, whether whether the callback itself throws an
--    exception, or whether the parent thread is thrown an asynchronous exception...
-- 2. ...by the time we get here, all threads created within the scope are guaranteed to have terminated.
--
-- Sometimes, a child thread may be performing an operation that is expected to sometimes fail; for this case, @ki@
-- provides 'Ki.async', which creates a thread that does not propagate any synchronous exceptions to its parent. Rather,
-- these exceptions are made available for the parent thread to 'Ki.await' and handle however it wishes.
--
-- Other times, it is considered very unexpected or erroneous for a child thread to fail; for this case, @ki@ provides
-- 'Ki.fork', which creates a thread that immediately propagates any synchronous exception it throws to its parent. The
-- intention is to facillitate "failing fast and loud" when there is little or nothing sensible for the programmer to do
-- besides propagate the exception up the call tree.
--
-- In either case, if a child thread is deliviered an asynchronous exception, it is immediately propagated to its
-- parent. This is in accordance with exception-handling best practices, which dictate that asynchronous exceptions
-- should never be ignored.
--
-- Each child thread can be thought to increases the "surface area" of the parent thread's identity, in the sense that
-- any synchronous or asynchronous exception thrown by or to a child will ultimately be re-thrown by or to the parent.

-- | Duration-based @threadDelay@.
sleep ::
  MonadIO m =>
  -- |
  Duration ->
  m ()
sleep duration =
  timeoutSTM duration (pure (pure ())) (pure ())
{-# SPECIALIZE sleep :: Duration -> IO () #-}
