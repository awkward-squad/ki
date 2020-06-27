{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Indef
  ( Cancelled (..),
    Context,
    Scope,
    Seconds,
    Thread,
    async,
    asyncWithUnmask,
    await,
    awaitFor,
    awaitSTM,
    cancel,
    cancelled,
    cancelledSTM,
    fork,
    forkWithUnmask,
    global,
    kill,
    scoped,
    timeout,
    wait,
    waitFor,
    waitSTM,
  )
where

import Ki.Indef.Seconds (Seconds)
import Ki.Indef.Thread (Thread, await, awaitFor, awaitSTM, kill, timeout)
import Ki.Internal.Concurrency
import qualified Ki.Internal.Context as Context
import Ki.Internal.Context (Cancelled (..), Context)
import Ki.Internal.Prelude
import Ki.Internal.Scope (Scope, cancel, global, scoped)
import qualified Ki.Internal.Scope as Scope

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
  (fmap . fmap) (throwIO . Cancelled_) (atomically (Context.cancelled ?context))

-- | @STM@ variant of 'cancelled'.
cancelledSTM :: Context => STM (Maybe (IO a))
cancelledSTM =
  (fmap . fmap) (throwIO . Cancelled_) (Context.cancelled ?context)

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
  timeout seconds (pure <$> Scope.wait scope) (pure ())

-- | @STM@ variant of 'wait'.
waitSTM :: Scope -> STM ()
waitSTM =
  Scope.wait
