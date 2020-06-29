{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit
  ( -- * Scope
    Scope,
    scoped,
    wait,
    waitSTM,
    waitFor,

    -- * Spawning threads

    -- ** Fork
    fork,
    forkWithUnmask,

    -- ** Actor
    Actor,
    send,

    -- ** Async
    Thread,
    async,
    asyncWithUnmask,
    await,
    awaitSTM,
    awaitFor,
    -- kill,

    -- * Soft-cancellation
    Context,
    cancel,
    cancelled,
    cancelledSTM,
    unlessCancelled,

    -- * Global context
    global,

    -- * Exceptions
    pattern Cancelled,

    -- * Miscellaneous
    Seconds,
    sleep,
    timeoutSTM,
  )
where

import Ki.Concurrency
import Ki.Implicit.Actor
import Ki.Implicit.Context (Context, cancelled, cancelledSTM, global, unlessCancelled, pattern Cancelled)
import qualified Ki.Implicit.Scope
import Ki.Implicit.Scope (Scope, cancel, scoped)
import Ki.Prelude
import Ki.Seconds (Seconds)
import Ki.Thread (Thread, await, awaitFor, awaitSTM)
import Ki.Timeout (timeoutSTM)

-- | Fork a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> (Context => IO a) -> IO (Thread a)
async scope action =
  Ki.Implicit.Scope.async scope \restore -> restore action

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
asyncWithUnmask scope action =
  Ki.Implicit.Scope.async scope \restore -> restore (action unsafeUnmask)

-- | Variant of 'async' that does not return a handle to the __thread__.
--
-- If the __thread__ throws an unexpected exception, the exception is propagated up the call tree to the __thread__ that
-- opened its __scope__.
--
-- There is one expected exception the __thread__ may throw that will not be propagated up the call tree:
--
--   * 'Cancelled', as when the __thread__ voluntarily capitulates after observing a /cancellation/ request.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: Scope -> (Context => IO ()) -> IO ()
fork scope action =
  Ki.Implicit.Scope.fork scope \restore -> restore action

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask scope action =
  Ki.Implicit.Scope.fork scope \restore -> restore (action unsafeUnmask)

-- | __Context__-aware @threadDelay@.
--
-- @
-- 'sleep' seconds =
--   'timeoutSTM seconds 'cancelledSTM' (pure ())
-- @
--
-- /Throws/:
--
--   * Throws 'Cancelled' if the current __context__ is /cancelled/.
sleep :: Context => Seconds -> IO ()
sleep seconds =
  timeoutSTM seconds cancelledSTM (pure ())

-- | Wait until all __threads__ forked within a __scope__ finish.
wait :: Scope -> IO ()
wait =
  atomically . Ki.Implicit.Scope.wait

-- | Variant of 'wait' that gives up after the given number of seconds elapses.
--
-- @
-- 'waitFor' scope seconds =
--   'timeout' seconds (pure \<$\> 'waitSTM' scope) (pure ())
-- @
waitFor :: Scope -> Seconds -> IO ()
waitFor scope seconds =
  timeoutSTM seconds (pure <$> Ki.Implicit.Scope.wait scope) (pure ())

-- | @STM@ variant of 'wait'.
waitSTM :: Scope -> STM ()
waitSTM =
  Ki.Implicit.Scope.wait
