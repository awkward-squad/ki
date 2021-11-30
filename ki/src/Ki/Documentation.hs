module Ki.Documentation
  ( -- * Introduction
    -- $introduction

    -- * Tutorial

    -- ** Background

    -- ** Structured concurrency
    -- $tutorial-structured-concurrency

    -- ** Creating threads
    -- $tutorial-creating-threads

    -- ** Exception propagation
    -- $tutorial-exception-propagation

    -- ** Soft-cancellation
    -- $tutorial-soft-cancellation

    -- * Examples
    -- $example-has-context-anchor

    -- ** Integrating ki with your application monad
    -- $example-has-context

    -- * Reference manual
    -- $reference-manual
  )
where

-- $introduction
--
-- This package provides two variants:
--
-- * "Ki" exposes a simplified version of the full API that does not include support for soft-cancellation. Thus, none
--   of the functions mention a __context__ type, and you may use any @MonadUnliftIO@-compatible monad, including plain
--   @IO@. If you do not intend to use soft-cancellation, there is no benefit to using the full API. Start here :)
--
-- * "Ki.Implicit" and "Ki.Reader" extend "Ki" with a __context__ type that's used to propagate soft-cancellation
--   signals. Because manually threading the right __context__ throughout a program is error-prone boilerplate,
--   package offers two ways of handling it implicitly: using implicit parameters, and using a reader monad.

-- $tutorial-structured-concurrency
--
-- $tutorial-soft-cancellation
--
-- An application may want to feature a "graceful teardown" procedure, which is preferred to a hard-shutdown as induced
-- by power loss or similar. The PostgreSQL server, for example, will first flush its write-ahead log to disk before
-- terminating, if it's sent a catchable kill signal such as SIGINT.
--
-- Similarly, a Web server may want to terminate a request-handler thread that has exceeded the amount of time the
-- server is willing to dedicate to handling the request, yet it would nonetheless prefer to allow the thread to clean
-- up and terminate on its own terms, to reduce the likelihood that the application is brought to a chaotic state.
--
-- For these use cases, @ki@ provides a "soft-cancellation" mechanism (in contrast to the "hard-cancellation" mechanism
-- provided by GHC, 'Control.Concurrent.throwTo'). A soft-cancelled thread may observe that it is soft-cancelled, but it
-- is under no obligation to respect the cancellation.
--
-- Let's see an example. First, from the perspective of a parent thread that opens a scope, creates a child thread, and
-- soft-cancels the scope.
--
-- @
-- 'Ki.Reader.scoped' \\scope -> do
--   -- __(1)__
--   'Ki.Reader.fork_' scope child
--   -- __(2)__
--   'Ki.Reader.cancel' scope
--   -- __(3)__
--   'Ki.Reader.waitFor' scope (5 * 'Ki.Reader.seconds')
--   -- __(4)__
-- @
--
-- 1. First, we create a child thread.
-- 2. Next, we cancel the scope. This allows the child (and its children, and so on) created within the scope to observe
--    the soft-cancellation.
-- 3. Finally, we wait for up to 5 seconds for the child to terminate. We elect not to wait indefinitely, to account for
--    the possibility that the child thread either does not ever observe the soft-cancellation signal, or does not
--    manage to perform its teardown procedure in a timely manner.
-- 4. Here, if the child is still alive, it will be hard-cancelled as the scope closes.

-- $example-has-context-anchor
--
-- #example_has_context#

-- $example-has-context
--
-- You may have an application monad that is defined similar to the following.
--
-- @
-- data Env
--   = Env
--   { ...
--   }
--
-- newtype App a
--   = App { runApp :: Env -> IO a }
--   deriving (MonadUnliftIO) via (ReaderT Env IO)
-- @
--
-- To use this module, first add one field to your @Env@ type that holds a __context__.
--
-- @
-- data Env
--   = Env
--   { ...
--   , envContext :: 'Ki.Reader.Context'
--   , ...
--   }
-- @
--
-- Then, write a 'Ki.Reader.HasContext' instance, which is a bit of boilerplate that encapsulates how to get and set
-- this field.
--
-- @
-- instance 'Ki.Reader.HasContext' App where
--   'Ki.Reader.askContext' =
--     App \\env -> pure (envContext env)
--
--   'Ki.Reader.withContext' context action =
--     App \\env -> runApp action env{ envContext = context }
-- @
--
-- And finally, when running your monad down to @IO@ in @main@ by providing an initial environment, use
-- 'Ki.Reader.globalContext'.
--
-- @
-- main :: IO ()
-- main =
--   runApp initialEnv action
--
-- initialEnv :: Env
-- initialEnv =
--   Env
--     { ...
--     , envContext = 'Ki.Reader.globalContext'
--     , ...
--     }
--
-- action :: App ()
-- action =
--   ...
-- @

-- $reference-manual
--
-- This reference manual contains implementation details for all of the major types and functions provided by the "full"
-- variant of this library (i.e. "Ki.Implicit" or "Ki.Reader"), which includes support for soft-cancellation.
--
-- The implementation of the stripped-down "Ki" variant is the same, but all references to cancellation and contexts can
-- simply be ignored.
--
-- ==== 'Ki.Reader.asyncWithUnmask' #reference_manual_async#
--
-- 'Ki.Reader.asyncWithUnmask' creates a thread within a scope.
--
-- If the scope is closed, this function calls 'error'. Otherwise, it creates a thread with the same masking state as
-- the thread that created it, and provides the thread with an @unmask@ function, which unmasks asynchronous exceptions.
--
-- The new thread is tracked in a data structure inside the scope, keyed by a monotonically increasing integer, so that
-- when the scope is closed, all living threads can be thrown an asynchronous exception in the order they were
-- created.
--
-- When the thread terminates, if it terminated with an exception, it first determines whether or not it should
-- propagate the exception to its parent.
--
--   - If the exception is a 'Ki.Internal.ScopeClosing', it is not propagated, as it is assumed to have come from the
--     parent thread directly.
--   - If the exception is a 'Ki.Internal.CancelToken' that was observable by the thread calling 'Ki.Reader.cancelled',
--     it is not propagated, as the parent thread either initiated the cancellation directly, or else some ancestor of
--     the parent thread initiated the cancellation (in which case the parent thread could observe the same cancel token
--     with 'Ki.Reader.cancelled'); either way, the parent thread could "know about" the cancellation, so it would be
--     incorrect to propagate this exception to the parent thread and induce an immediate termination of the thread's
--     siblings.
--   - If the exception is asynchronous (e.g. a subclass of 'Control.Exception.SomeAsyncException'), it is propagated.
--   - Otherwise, the exception is not propagated.
--
-- The thread's result is then made available via 'Ki.Reader.await', and finally the thread removes itself from the data
-- structure in its scope that tracks its existence, because the purpose of the data structure is to track all threads
-- still running within a scope, so they can all be terminated when the scope closes.
