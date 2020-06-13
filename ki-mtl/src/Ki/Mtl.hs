{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Ki.Mtl
  ( MonadKi,

    -- * Context
    Context,
    background,
    cancelled,
    cancelledSTM,

    -- * Scope
    Scope,
    scoped,
    wait,
    waitFor,
    waitSTM,
    cancel,

    -- * Thread
    Thread,
    async,
    asyncWithUnmask,
    fork,
    forkWithUnmask,
    await,
    awaitFor,
    awaitSTM,
    kill,

    -- * Exceptions
    Cancelled (..),

    -- * Miscellaneous
    Seconds,
    timeout,
  )
where

import Control.Exception (SomeException)
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Generics.Product.Typed (HasType (getTyped, setTyped))
import GHC.Conc (STM)
import Ki (Cancelled (..), Context, Scope, Seconds, Thread, awaitSTM, background, waitSTM)
import qualified Ki

-- | A convenience type alias for classifying monads suitable for use with @ki@.
type MonadKi r m =
  (HasType Context r, MonadReader r m, MonadUnliftIO m)

-- | Fork a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: MonadKi r m => Scope -> m a -> m (Thread a)
async scope k =
  withRunInIO \unlift ->
    Ki.async scope \context ->
      unlift (local (setTyped context) k)

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: MonadKi r m => Scope -> ((forall x. m x -> m x) -> m a) -> m (Thread a)
asyncWithUnmask scope k =
  withRunInIO \unlift ->
    Ki.asyncWithUnmask scope \context unmask ->
      unlift (local (setTyped context) (k \m -> liftIO (unmask (unlift m))))

-- | Wait for a __thread__ to finish.
await :: MonadIO m => Thread a -> m (Either SomeException a)
await =
  liftIO . Ki.await

-- | Variant of 'await' that gives up after the given number of seconds elapses.
--
-- @
-- 'awaitFor' thread seconds =
--   'timeout' seconds (pure . Just \<$\> 'awaitSTM' thread) (pure Nothing)
-- @
awaitFor :: MonadIO m => Thread a -> Seconds -> m (Maybe (Either SomeException a))
awaitFor thread seconds =
  liftIO (Ki.awaitFor thread seconds)

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel :: MonadIO m => Scope -> m ()
cancel =
  liftIO . Ki.cancel

-- | Return whether a __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The returned action may be
-- used to honor the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with a
-- value.
--
-- ==== __Examples__
--
-- Sometimes, a __thread__ may terminate with a value after observing a cancellation request.
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
cancelled :: MonadKi r m => m (Maybe (m a))
cancelled = do
  context <- askContext
  (fmap . fmap) liftIO (liftIO (Ki.cancelled context))

-- | @STM@ variant of 'cancelled'.
cancelledSTM :: MonadKi r m => m (STM (Maybe (m a)))
cancelledSTM = do
  context <- askContext
  pure ((fmap . fmap) liftIO (Ki.cancelledSTM context))

-- | Variant of 'async' that does not return a handle to the __thread__.
--
-- If the forked __thread__ throws an /unexpected/ exception, the exception is propagated up the call tree to the
-- __thread__ that opened its __scope__.
--
-- There is one /expected/ exceptions a __thread__ may throw that will not be propagated up the call tree:
--
--   * 'Cancelled', as when a __thread__ voluntarily capitulates after observing a /cancellation/ request.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: MonadKi r m => Scope -> m () -> m ()
fork scope k =
  withRunInIO \unlift ->
    Ki.fork scope \context ->
      unlift (local (setTyped context) k)

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: MonadKi r m => Scope -> ((forall x. m x -> m x) -> m ()) -> m ()
forkWithUnmask scope k =
  withRunInIO \unlift ->
    Ki.forkWithUnmask scope \context unmask ->
      unlift (local (setTyped context) (k \m -> liftIO (unmask (unlift m))))

-- | Kill a __thread__ wait for it to finish.
--
-- /Throws/:
--
--   * 'ThreadKilled' if a __thread__ attempts to kill itself.
kill :: MonadIO m => Thread a -> m ()
kill =
  liftIO . Ki.kill

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
scoped :: MonadKi r m => (Scope -> m a) -> m a
scoped k = do
  context <- askContext
  withRunInIO \unlift ->
    Ki.scoped context \scope ->
      unlift (k scope)

-- | Wait for an @STM@ action to return, and return the action contained within.
--
-- If the given number of seconds elapse, return the given action instead.
timeout :: MonadUnliftIO m => Seconds -> STM (m a) -> m a -> m a
timeout seconds action fallback =
  withRunInIO \unlift ->
    liftIO (Ki.timeout seconds (unlift <$> action) (unlift fallback))

-- | Wait until all __threads__ within a __scope__ finish.
wait :: MonadIO m => Scope -> m ()
wait =
  liftIO . Ki.wait

-- | Variant of 'wait' that gives up after the given number of seconds elapses.
--
-- @
-- 'waitFor' scope seconds =
--   'timeout' seconds (pure \<$\> 'waitSTM' scope) (pure ())
-- @
waitFor :: MonadIO m => Scope -> Seconds -> m ()
waitFor scope seconds =
  liftIO (Ki.waitFor scope seconds)

--- Helpers

askContext :: (HasType Context r, MonadReader r m) => m Context
askContext =
  asks getTyped
