{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Ki.Mtl
  ( -- * Context
    Context,
    background,
    cancelled,
    cancelledSTM,

    -- * Scope
    Scope,
    scoped,
    wait,
    waitSTM,
    waitFor,

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

async ::
  (HasType Context r, MonadReader r m, MonadUnliftIO m) =>
  Scope ->
  m a ->
  m (Thread a)
async scope k =
  withRunInIO \unlift ->
    Ki.async scope \context ->
      unlift (local (setTyped context) k)

async_ ::
  (HasType Context r, MonadReader r m, MonadUnliftIO m) =>
  Scope ->
  m a ->
  m ()
async_ scope k =
  withRunInIO \unlift ->
    Ki.async_ scope \context ->
      unlift (local (setTyped context) k)

asyncWithUnmask ::
  (HasType Context r, MonadReader r m, MonadUnliftIO m) =>
  Scope ->
  ((forall x. m x -> m x) -> m a) ->
  m (Thread a)
asyncWithUnmask scope k =
  withRunInIO \unlift ->
    Ki.asyncWithUnmask scope \context unmask ->
      unlift (local (setTyped context) (k \m -> liftIO (unmask (unlift m))))

asyncWithUnmask_ ::
  (HasType Context r, MonadReader r m, MonadUnliftIO m) =>
  Scope ->
  ((forall x. m x -> m x) -> m a) ->
  m ()
asyncWithUnmask_ scope k =
  withRunInIO \unlift ->
    Ki.asyncWithUnmask_ scope \context unmask ->
      unlift (local (setTyped context) (k \m -> liftIO (unmask (unlift m))))

await :: MonadIO m => Thread a -> m a
await =
  liftIO . Ki.await

kill :: MonadIO m => Thread a -> m ()
kill =
  liftIO . Ki.kill

cancelled :: (HasType Context r, MonadIO m, MonadReader r m) => m Bool
cancelled = do
  context <- askContext
  liftIO (Ki.cancelled context)

cancelledSTM :: (HasType Context r, MonadReader r m) => m (STM Bool)
cancelledSTM = do
  context <- askContext
  pure (Ki.cancelledSTM context)

scoped ::
  (HasType Context r, MonadReader r m, MonadUnliftIO m) =>
  (Scope -> m a) ->
  m a
scoped k = do
  context <- askContext
  withRunInIO \unlift ->
    Ki.scoped context \scope ->
      unlift (k scope)

wait :: MonadIO m => Scope -> m ()
wait =
  liftIO . Ki.wait

waitFor :: MonadIO m => Scope -> Int -> m ()
waitFor scope micros =
  liftIO (Ki.waitFor scope micros)

--- Helpers

askContext :: (HasType Context r, MonadReader r m) => m Context
askContext =
  asks getTyped
