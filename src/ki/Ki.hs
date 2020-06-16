{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- The only reason it module exists is:
--
-- 1. Haddock doesn't show reexported-modules
-- 2. Even if it did, haddock doesn't seem to preserve comments through backpack signatures
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
    K.Cancelled (Cancelled),

    -- * Miscellaneous
    Seconds,
    timeout,
  )
where

import Control.Exception (SomeException)
import Data.Coerce (coerce)
import Data.Data (Data)
import GHC.Conc (STM)
import GHC.Generics (Generic)
import qualified Ki.Indef as K

-- | A 'Cancelled' exception is thrown when a __thread__ voluntarily capitulates after observing its __context__ is
-- /cancelled/.
pattern Cancelled :: K.Cancelled
pattern Cancelled <- K.Cancelled_ _

{-# COMPLETE Cancelled #-}

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ forked within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
type Context
  = K.Context

-- | A __scope__ delimits the lifetime of all __threads__ forked within it. A __thread__ cannot outlive its __scope__.
--
-- When a __scope__ is /closed/, all remaining __threads__ forked within it are killed.
--
-- The basic usage of a __scope__ is as follows.
--
-- @
-- 'scoped' \\scope -> do
--   'fork' scope worker1
--   'fork' scope worker2
--   'wait' scope
-- @
--
-- A __scope__ can be passed into functions or shared amongst __threads__, but this is generally not advised, as it
-- takes the "structure" out of "structured concurrency".
newtype Scope
  = Scope K.Scope

newtype Seconds
  = Seconds K.Seconds
  deriving stock (Data, Generic)
  deriving newtype (Enum, Eq, Fractional, Num, Ord, Read, Real, RealFrac, Show)

-- | A running __thread__.
newtype Thread a
  = Thread (K.Thread a)
  deriving stock (Generic)
  deriving newtype (Eq, Ord)

-- | Fork a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> (Context => IO a) -> IO (Thread a)
async scope action = coerce (K.async (coerce scope) action)

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
asyncWithUnmask scope k = coerce (K.asyncWithUnmask (coerce scope) k)

-- | Wait for a __thread__ to finish.
await :: Thread a -> IO (Either SomeException a)
await = _await

_await :: forall a. Thread a -> IO (Either SomeException a)
_await = coerce (K.await @a)

-- | Variant of 'await' that gives up after the given number of seconds elapses.
--
-- @
-- 'awaitFor' thread seconds =
--   'timeout' seconds (pure . Just \<$\> 'awaitSTM' thread) (pure Nothing)
-- @
awaitFor :: Thread a -> Seconds -> IO (Maybe (Either SomeException a))
awaitFor = _awaitFor

_awaitFor :: forall a. Thread a -> Seconds -> IO (Maybe (Either SomeException a))
_awaitFor = coerce (K.awaitFor @a)

-- | @STM@ variant of 'await'.
--
-- /Throws/:
--
--   * The exception that the __thread__ threw, if any.
awaitSTM :: Thread a -> STM (Either SomeException a)
awaitSTM = _awaitSTM

_awaitSTM :: forall a. Thread a -> STM (Either SomeException a)
_awaitSTM = coerce (K.awaitSTM @a)

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel :: Scope -> IO ()
cancel = coerce K.cancel

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
cancelled = K.cancelled

-- | @STM@ variant of 'cancelled'.
cancelledSTM :: Context => STM (Maybe (IO a))
cancelledSTM = K.cancelledSTM

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
fork scope = K.fork (coerce scope)

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask scope = K.forkWithUnmask (coerce scope)

-- | Kill a __thread__ wait for it to finish.
--
-- /Throws/:
--
--   * 'ThreadKilled' if a __thread__ attempts to kill itself.
kill :: Thread a -> IO ()
kill = _kill

_kill :: forall a. Thread a -> IO ()
_kill = coerce (K.kill @a)

-- | Run an action in the global __context__.
--
-- You should only call this function if there is not already a __context__ in scope, as in @main@.
global :: (Context => IO a) -> IO a
global = K.global

-- | Perform an action with a new __scope__, then /close/ the __scope__.
--
-- /Throws/:
--
--   * The first exception a __thread__ forked with 'fork' throws, if any.
--
-- ==== __Examples__
--
-- @
-- 'scoped' \\scope -> do
--   'fork' scope worker1
--   'fork' scope worker2
--   'wait' scope
-- @
scoped :: Context => (Context => Scope -> IO a) -> IO a
scoped action = K.scoped \scope -> action (coerce scope)

-- | Wait for an @STM@ action to return, and return the @IO@ action contained within.
--
-- If the given number of seconds elapses, return the given @IO@ action instead.
timeout :: Seconds -> STM (IO a) -> IO a -> IO a
timeout = _timeout

_timeout :: forall a. Seconds -> STM (IO a) -> IO a -> IO a
_timeout = coerce (K.timeout @a)

-- | Variant of 'wait' that gives up after the given number of seconds elapses.
--
-- @
-- 'waitFor' scope seconds =
--   'timeout' seconds (pure \<$\> 'waitSTM' scope) (pure ())
-- @
waitFor :: Scope -> Seconds -> IO ()
waitFor = coerce K.waitFor

-- | Wait until all __threads__ forked within a __scope__ finish.
wait :: Scope -> IO ()
wait = coerce K.wait

-- | @STM@ variant of 'wait'.
waitSTM :: Scope -> STM ()
waitSTM = coerce K.waitSTM
