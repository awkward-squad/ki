{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- The only reason it module exists is:
--
-- 1. Haddock doesn't show reexported-modules
-- 2. Even if it did, haddock doesn't seem to preserve comments through
--    backpack signatures
module Ki
  ( -- * Context
    Context,
    background,
    CancelToken,
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
    async_,
    asyncWithUnmask,
    asyncWithUnmask_,
    await,
    awaitSTM,
    awaitFor,
    kill,

    -- * Exceptions
    Cancelled (..),

    -- * Miscellaneous
    Seconds,
    timeout,
  )
where

import Control.Exception (Exception)
import Data.Coerce (coerce)
import GHC.Conc (STM)
import GHC.Generics (Generic)
import qualified Ki.Indef as K

newtype Cancelled
  = Cancelled K.Cancelled
  deriving newtype (Eq, Exception, Show)

newtype CancelToken
  = CancelToken K.CancelToken
  deriving newtype (Eq, Show)

-- | A __context__ models a program's call tree.
--
-- Every __thread__ has its own __context__, which is used as a mechanism to
-- propagate /cancellation/.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is
-- a suggestion to perform a graceful shutdown and finish.
newtype Context
  = Context K.Context
  deriving stock (Generic)

-- | A __scope__ delimits the lifetime of all __threads__ forked within it.
--
-- * When a __scope__ is /closed/, all remaining __threads__ are killed.
-- * If a __thread__ throws an exception, its __scope__ is /closed/.
--
-- A __scope__ can be passed into functions or shared amongst __threads__, but
-- this is generally not advised, as it takes the "structure" out of "structured
-- concurrency".
--
-- The basic usage of a __scope__ is as follows:
--
-- @
-- 'scoped' context \\scope -> do
--   'async_' scope worker1
--   'async_' scope worker2
--   'wait' scope
-- @
newtype Scope
  = Scope K.Scope

newtype Seconds
  = Seconds K.Seconds

-- | A running __thread__.
newtype Thread a
  = Thread (K.Thread a)
  deriving stock (Generic)
  deriving newtype (Eq, Ord)

-- | Fork a __thread__ within a __scope__. The derived __context__ should
-- replace the usage of any other __context__ in scope.
--
-- /Throws/:
--
--   * Calls 'error' the __scope__ is /closed/.
async :: Scope -> (Context -> IO a) -> IO (Thread a)
async = _async
{-# INLINE async #-}

_async :: forall a. Scope -> (Context -> IO a) -> IO (Thread a)
_async = coerce @(K.Scope -> (K.Context -> IO a) -> IO (K.Thread a)) K.async
{-# INLINE _async #-}

-- | Fire-and-forget variant of 'async'.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async_ :: Scope -> (Context -> IO a) -> IO ()
async_ = _async_
{-# INLINE async_ #-}

_async_ :: forall a. Scope -> (Context -> IO a) -> IO ()
_async_ = coerce @(K.Scope -> (K.Context -> IO a) -> IO ()) K.async_
{-# INLINE _async_ #-}

-- | Variant of 'async' that provides the __thread__ a function that unmasks
-- asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask ::
  Scope ->
  (Context -> (forall x. IO x -> IO x) -> IO a) ->
  IO (Thread a)
asyncWithUnmask = _asyncWithUnmask
{-# INLINE asyncWithUnmask #-}

_asyncWithUnmask ::
  forall a.
  Scope ->
  (Context -> (forall x. IO x -> IO x) -> IO a) ->
  IO (Thread a)
_asyncWithUnmask scope k =
  coerce
    @(IO (K.Thread a))
    (K.asyncWithUnmask (coerce scope) \context -> k (coerce context))
{-# INLINE _asyncWithUnmask #-}

-- | Fire-and-forget variant of 'asyncWithUnmask'.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask_ ::
  Scope ->
  (Context -> (forall x. IO x -> IO x) -> IO a) ->
  IO ()
asyncWithUnmask_ scope k =
  K.asyncWithUnmask_ (coerce scope) \context -> k (coerce context)

-- | Wait for a __thread__ to finish.
--
-- /Throws/:
--
--   * The exception that the __thread__ threw, if any.
await :: Thread a -> IO a
await = _await
{-# INLINE await #-}

_await :: forall a. Thread a -> IO a
_await = coerce @(K.Thread a -> IO a) K.await
{-# INLINE _await #-}

-- | Variant of 'await' that gives up after the given number of seconds elapses.
--
-- @
-- 'awaitFor' thread seconds =
--   'timeout' seconds (pure . Just \<$\> 'awaitSTM' thread) (pure Nothing)
-- @
awaitFor :: Thread a -> Seconds -> IO (Maybe a)
awaitFor = _awaitFor
{-# INLINE awaitFor #-}

_awaitFor :: forall a. Thread a -> Seconds -> IO (Maybe a)
_awaitFor = coerce @(K.Thread a -> K.Seconds -> IO (Maybe a)) K.awaitFor
{-# INLINE _awaitFor #-}

-- | @STM@ variant of 'await'.
--
-- /Throws/:
--
--   * The exception that the __thread__ threw, if any.
awaitSTM :: Thread a -> STM a
awaitSTM = _awaitSTM
{-# INLINE awaitSTM #-}

_awaitSTM :: forall a. Thread a -> STM a
_awaitSTM = coerce @(K.Thread a -> STM a) K.awaitSTM
{-# INLINE _awaitSTM #-}

-- | The background __context__.
--
-- You should only use this when another __context__ isn't available, as when
-- creating a top-level __scope__ from the main thread.
--
-- The background __context__ cannot be /cancelled/.
background :: Context
background = coerce K.background
{-# INLINE background #-}

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel :: Scope -> IO ()
cancel = coerce K.cancel
{-# INLINE cancel #-}

-- | Return whether a __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ will be killed soon; they
-- should attempt to perform a graceful shutdown and finish.
cancelled :: Context -> IO (Maybe CancelToken)
cancelled = coerce K.cancelled
{-# INLINE cancelled #-}

-- | @STM@ variant of 'cancelled'.
cancelledSTM :: Context -> STM (Maybe CancelToken)
cancelledSTM = coerce K.cancelledSTM
{-# INLINE cancelledSTM #-}

-- | Kill a __thread__ wait for it to finish.
--
-- /Throws/:
--
--   * 'ThreadKilled' if a __thread__ attempts to kill itself.
kill :: Thread a -> IO ()
kill = _kill
{-# INLINE kill #-}

_kill :: forall a. Thread a -> IO ()
_kill = coerce @(K.Thread a -> IO ()) K.kill
{-# INLINE _kill #-}

-- | Perform an action with a new __scope__, then /close/ the __scope__.
--
-- /Throws/:
--
--   * The first exception a __thread__ forked within the __scope__ throws, if
--     any.
--
-- ==== __Examples__
--
-- @
-- 'scoped' context \\scope -> do
--   'async_' scope worker1
--   'async_' scope worker2
--   'wait' scope
-- @
scoped :: Context -> (Scope -> IO a) -> IO a
scoped = _scoped
{-# INLINE scoped #-}

_scoped :: forall a. Context -> (Scope -> IO a) -> IO a
_scoped = coerce @(K.Context -> (K.Scope -> IO a) -> IO a) K.scoped
{-# INLINE _scoped #-}

-- | Wait for an @STM@ action to return, and return the @IO@ action contained
-- within.
--
-- If the given number of seconds elapse, return the given @IO@ action instead.
timeout :: Seconds -> STM (IO a) -> IO a -> IO a
timeout = _timeout
{-# INLINE timeout #-}

_timeout :: forall a. Seconds -> STM (IO a) -> IO a -> IO a
_timeout = coerce @(K.Seconds -> STM (IO a) -> IO a -> IO a) K.timeout
{-# INLINE _timeout #-}

-- | Variant of 'wait' that gives up after the given number of seconds elapses.
--
-- @
-- 'waitFor' scope seconds =
--   'timeout' seconds (pure \<$\> 'waitSTM' scope) (pure ())
-- @
waitFor :: Scope -> Seconds -> IO ()
waitFor = coerce K.waitFor
{-# INLINE waitFor #-}

-- | Wait until all __threads__ forked within a __scope__ finish.
wait :: Scope -> IO ()
wait = coerce K.wait
{-# INLINE wait #-}

-- | @STM@ variant of 'wait'.
waitSTM :: Scope -> STM ()
waitSTM = coerce K.waitSTM
{-# INLINE waitSTM #-}
