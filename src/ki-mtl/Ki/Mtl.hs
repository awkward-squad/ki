{-# LANGUAGE BlockArguments #-}
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
    waitSTM,

    -- * Thread
    Thread,
    async,
    async_,
    asyncWithUnmask,
    asyncWithUnmask_,
    await,
    awaitSTM,
    kill,

    -- * Exceptions
    ScopeClosed (..),
    ThreadFailed (..),
  )
where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Generics.Product.Typed (HasType (getTyped, setTyped))
import GHC.Conc (STM)
import Ki (Context, Scope, ScopeClosed (..), Thread, ThreadFailed (..), awaitSTM, background, waitSTM)
import qualified Ki

-- | A convenience type alias for classifying monads suitable for use with @ki@.
type MonadKi r m =
  (HasType Context r, MonadReader r m, MonadUnliftIO m)

-- | Fork a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * 'ScopeClosed' if the __scope__ is /closed/.
async :: MonadKi r m => Scope -> m a -> m (Thread a)
async scope k =
  withRunInIO \unlift ->
    Ki.async scope \context ->
      unlift (local (setTyped context) k)

-- | Fire-and-forget variant of 'async'.
--
-- /Throws/:
--
--   * 'ScopeClosed' if the __scope__ is /closed/.
async_ :: MonadKi r m => Scope -> m a -> m ()
async_ scope k =
  withRunInIO \unlift ->
    Ki.async_ scope \context ->
      unlift (local (setTyped context) k)

-- | Variant of 'async' that provides the __thread__ a function that unmasks
-- asynchronous exceptions.
--
-- /Throws/:
--
--   * 'ScopeClosed' if the __scope__ is /closed/.
asyncWithUnmask ::
  MonadKi r m =>
  Scope ->
  ((forall x. m x -> m x) -> m a) ->
  m (Thread a)
asyncWithUnmask scope k =
  withRunInIO \unlift ->
    Ki.asyncWithUnmask scope \context unmask ->
      unlift (local (setTyped context) (k \m -> liftIO (unmask (unlift m))))

-- | Fire-and-forget variant of 'asyncWithUnmask'.
--
-- /Throws/:
--
--   * 'ScopeClosed' if the __scope__ is /closed/.
asyncWithUnmask_ ::
  MonadKi r m =>
  Scope ->
  ((forall x. m x -> m x) -> m a) ->
  m ()
asyncWithUnmask_ scope k =
  withRunInIO \unlift ->
    Ki.asyncWithUnmask_ scope \context unmask ->
      unlift (local (setTyped context) (k \m -> liftIO (unmask (unlift m))))

-- | Wait for a __thread__ to finish.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ threw an exception.
await :: MonadIO m => Thread a -> m a
await =
  liftIO . Ki.await

-- | Kill a __thread__ wait for it to finish.
--
-- /Throws/:
--
--   * 'ThreadKilled' if a __thread__ attempts to kill itself.
kill :: MonadIO m => Thread a -> m ()
kill =
  liftIO . Ki.kill

-- | Return whether the __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ will be killed soon; they
-- should attempt to perform a graceful shutdown and finish.
cancelled :: MonadKi r m => m Bool
cancelled = do
  context <- askContext
  liftIO (Ki.cancelled context)

-- | @STM@ variant of 'cancelled'.
cancelledSTM :: MonadKi r m => m (STM Bool)
cancelledSTM = do
  context <- askContext
  pure (Ki.cancelledSTM context)

-- | Perform an action with a new __scope__, then /close/ the __scope__.
--
-- /Throws/:
--
--   * 'ThreadFailed' if a __thread__ throws an exception.
--
-- ==== __Examples__
--
-- @
-- 'scoped' \\scope -> do
--   'async_' scope worker1
--   'async_' scope worker2
--   'wait' scope
-- @
scoped :: MonadKi r m => (Scope -> m a) -> m a
scoped k = do
  context <- askContext
  withRunInIO \unlift ->
    Ki.scoped context \scope ->
      unlift (k scope)

-- | Wait until all __threads__ within a __scope__ finish.
wait :: MonadIO m => Scope -> m ()
wait =
  liftIO . Ki.wait

--- Helpers

askContext :: (HasType Context r, MonadReader r m) => m Context
askContext =
  asks getTyped
