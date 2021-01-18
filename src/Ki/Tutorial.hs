module Ki.Tutorial
  ( -- * Introduction
    -- $introduction
  )
where

-- $introduction
--
-- In GHC Haskell, a background thread can be spawned with "Control.Concurrent.forkIO" at any time, and its lifetime is 
-- wholly disconnected from the lifetime of its parent: if the child dies, it has no effect whatsoever on the parent, 
-- and vice-versa. It is hereafter the programmer's responsibility to manage the two parallel threads of execution.
--
-- Commonly, though, there is some higher-level structure to this relationship between parent and child, and we'd like
-- to codify that relationship in a general way that permits reuse. For example, the notion that a child thread has a 
-- "return value" is an idea explored in many ecosystems and packages, including the venerable @async@ Haskell package, 
-- wherein a child thread can be "awaited", which is an operation that blocks until the child thread terminates with a 
-- value or an exception.
--
-- Here it is supposed, but not required, that whatever parent thread is creating child threads is also awaiting their 
-- termination.
--
-- And when a child thread is though to have a "return value", we of course have to reason through how our program 
-- should behave if that return value never materializes, because the child thread threw (or, in Haskell, was thrown) an
-- exception.
-- 
-- Sometimes, it might be erroneous for a parent thread to continue running if one of its children has thrown an
-- exception, so we might endeavor to eagerly throw an asynchronous exception from child to parent in this case, so the
-- exception is noticed in a timely manner.
--
-- Other times, it would not make sense
--
--
--
--
--
--
--
-- it would not it would not make sense for a child thread to continue running
-- if its parent is no longer alive, so we might endeavor to kill all of a thread's children just before it terminates.
--
-- These ideas and more have been explored in various Haskell libraries that are built on top of @forkIO@, most notably
-- the venerable @async@ library, which has an entire book authored by Simon Marlowe that motivates its design.
--
-- Structured concurrency takes the idea of a sensible parent-child relationship one step further by requiring _all_
-- concurrently running threads to be created within some "scope", such that when the scope exits, or closes, or ceases
-- to exist for one reason or another, all threads that were created within it, if any, are terminated. In short, a
-- function cannot create
--

-- Structured concurrency, in a nutshell, is a restricted style of programming wherein a thread is prevented from 
-- outliving some sort of scope in which it was created.
