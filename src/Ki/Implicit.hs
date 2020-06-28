{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit
  ( -- * Context
    Context,
    global,
    cancelled,
    cancelledSTM,
    unlessCancelled,

    -- * Scope
    Scope,
    scoped,
    wait,
    waitSTM,
    waitFor,
    cancel,

    -- * Thread
    Thread,
    async,
    asyncWithUnmask,
    fork,
    forkWithUnmask,
    await,
    awaitSTM,
    awaitFor,
    kill,

    -- * Exceptions
    pattern Cancelled,

    -- * Miscellaneous
    Seconds,
    timeoutSTM,
  )
where

import qualified Ki.Implicit.Internal.Context
import Ki.Implicit.Internal.Context (Context, global, pattern Cancelled)
import qualified Ki.Implicit.Internal.Scope
import Ki.Implicit.Internal.Scope (Scope, cancel, scoped)
import Ki.Internal.Concurrency
import Ki.Internal.Prelude
import Ki.Internal.Seconds (Seconds)
import Ki.Internal.Thread (Thread, await, awaitFor, awaitSTM, kill, timeoutSTM)

-- | Fork a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> (Context => IO a) -> IO (Thread a)
async scope action =
  Ki.Implicit.Internal.Scope.async scope \restore -> restore action

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
asyncWithUnmask scope action =
  Ki.Implicit.Internal.Scope.async scope \restore -> restore (action unsafeUnmask)

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The returned action may be
-- used to honor the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with a
-- value.
--
-- Sometimes, a __thread__ may terminate with a value after observing a /cancellation/ request.
--
-- @
-- 'cancelled' >>= \\case
--   Nothing -> continue
--   Just _capitulate -> do
--     cleanup
--     pure value
-- @
--
-- Other times, it may be unable to, so it should call the provided action.
--
-- @
-- 'cancelled' >>= \\case
--   Nothing -> continue
--   Just capitulate -> do
--     cleanup
--     capitulate
-- @
--
-- But commonly, a thread has no explicit teardown phase, and can immediately honor a /cancellation/ request;
-- 'unlessCancelled' is useful for that.
--
-- @
-- 'unlessCancelled' continue
-- @
cancelled :: Context => IO (Maybe (IO a))
cancelled =
  atomically (optional cancelledSTM)

-- | @STM@ variant of 'cancelled'.
cancelledSTM :: Context => STM (IO a)
cancelledSTM =
  Ki.Implicit.Internal.Context.cancelled ?context

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
  Ki.Implicit.Internal.Scope.fork scope \restore -> restore action

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask scope action =
  Ki.Implicit.Internal.Scope.fork scope \restore -> restore (action unsafeUnmask)

-- | Variant of 'cancelled' that immediately capitulates if the current __context__ is /cancelled/.
--
-- @
-- 'unlessCancelled' action =
--   'cancelled' >>= \\case
--     Nothing -> action
--     Just capitulate -> capitulate
-- @
--
-- /Throws/:
--
--   * Throws 'Cancelled' if the current __context__ is /cancelled/.
unlessCancelled :: Context => IO a -> IO a
unlessCancelled action =
  cancelled >>= fromMaybe action

-- | Wait until all __threads__ forked within a __scope__ finish.
wait :: Scope -> IO ()
wait =
  atomically . Ki.Implicit.Internal.Scope.wait

-- | Variant of 'wait' that gives up after the given number of seconds elapses.
--
-- @
-- 'waitFor' scope seconds =
--   'timeout' seconds (pure \<$\> 'waitSTM' scope) (pure ())
-- @
waitFor :: Scope -> Seconds -> IO ()
waitFor scope seconds =
  timeoutSTM seconds (pure <$> Ki.Implicit.Internal.Scope.wait scope) (pure ())

-- | @STM@ variant of 'wait'.
waitSTM :: Scope -> STM ()
waitSTM =
  Ki.Implicit.Internal.Scope.wait
