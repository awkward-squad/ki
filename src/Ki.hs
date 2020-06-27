{-# LANGUAGE PatternSynonyms #-}

module Ki
  ( -- * Context
    Context,
    global,
    cancelled,
    cancelledSTM,

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
    Context.Cancelled (Cancelled),

    -- * Miscellaneous
    Seconds,
    timeoutSTM,
  )
where

import Ki.Internal.Concurrency
import qualified Ki.Internal.Context as Context
import Ki.Internal.Context (Context, global)
import Ki.Internal.Prelude
import Ki.Internal.Scope (Scope, cancel, scoped)
import qualified Ki.Internal.Scope as Scope
import Ki.Internal.Seconds (Seconds)
import Ki.Internal.Thread (Thread, await, awaitFor, awaitSTM, kill, timeoutSTM)

-- | A 'Cancelled' exception is thrown when a __thread__ voluntarily capitulates after observing its __context__ is
-- /cancelled/.
pattern Cancelled :: Context.Cancelled
pattern Cancelled <- Context.Cancelled_ _

{-# COMPLETE Cancelled #-}

-- | Fork a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> (Context => IO a) -> IO (Thread a)
async scope action =
  Scope.async scope \restore -> restore action

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
asyncWithUnmask scope action =
  Scope.async scope \restore -> restore (action unsafeUnmask)

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The returned action may be
-- used to honor the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with a
-- value.
--
-- ==== __Examples__
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
cancelled :: Context => IO (Maybe (IO a))
cancelled =
  (fmap . fmap) (throwIO . Context.Cancelled_) (atomically (Context.cancelled ?context))

-- | @STM@ variant of 'cancelled'.
cancelledSTM :: Context => STM (Maybe (IO a))
cancelledSTM =
  (fmap . fmap) (throwIO . Context.Cancelled_) (Context.cancelled ?context)

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
  Scope.fork scope \restore -> restore action

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask scope action =
  Scope.fork scope \restore -> restore (action unsafeUnmask)

-- | Wait until all __threads__ forked within a __scope__ finish.
wait :: Scope -> IO ()
wait =
  atomically . Scope.wait

-- | Variant of 'wait' that gives up after the given number of seconds elapses.
--
-- @
-- 'waitFor' scope seconds =
--   'timeout' seconds (pure \<$\> 'waitSTM' scope) (pure ())
-- @
waitFor :: Scope -> Seconds -> IO ()
waitFor scope seconds =
  timeoutSTM seconds (pure <$> Scope.wait scope) (pure ())

-- | @STM@ variant of 'wait'.
waitSTM :: Scope -> STM ()
waitSTM =
  Scope.wait
