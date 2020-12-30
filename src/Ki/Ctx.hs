{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Ctx
  ( Ctx (..),
    CancelState (..),
    CancelWay (..),
    newCtxSTM,
    deriveCtx,
    cancelCtx,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Ki.CancelToken
import Ki.Prelude

data Ctx = Ctx
  { ctx'cancelStateVar :: TVar CancelState,
    ctx'childrenVar :: TVar (IntMap Ctx),
    -- | The next id to assign to a child context. The child needs a unique identifier so it can delete itself from its
    -- parent's children map if it's cancelled independently. Wrap-around seems ok; that's a *lot* of children for one
    -- parent to have.
    ctx'nextIdVar :: TVar Int,
    -- | Remove myself from my parent's context. This isn't simply a pointer to the parent 'Ctx' for three reasons:
    --
    --   * "Root" contexts don't have a parent, so it'd have to be a Maybe (one more pointer indirection)
    --   * We don't really need a reference to the parent, because we only want to be able to remove ourselves from its
    --     children map, so just storing the STM action that does exactly seems a bit safer, even if conceptually it's
    --     a bit indirect.
    --   * If we stored a reference to the parent, we'd also have to store our own id, rather than just currying it into
    --     this action.
    ctx'removeFromParent :: STM ()
  }

data CancelState
  = CancelState'NotCancelled
  | CancelState'Cancelled CancelToken CancelWay

data CancelWay
  = CancelWay'Indirect -- parent (or grandparent, etc...) was cancelled
  | CancelWay'Direct -- we were cancelled

newCtxSTM :: STM Ctx
newCtxSTM = do
  cancelStateVar <- newTVar CancelState'NotCancelled
  newCtxSTM_ cancelStateVar (pure ())

newCtxSTM_ :: TVar CancelState -> STM () -> STM Ctx
newCtxSTM_ ctx'cancelStateVar ctx'removeFromParent = do
  ctx'childrenVar <- newTVar IntMap.empty
  ctx'nextIdVar <- newTVar 0
  pure Ctx {ctx'cancelStateVar, ctx'childrenVar, ctx'nextIdVar, ctx'removeFromParent}

deriveCtx :: Ctx -> STM Ctx
deriveCtx ctx = do
  childId <- readTVar (ctx'nextIdVar ctx)
  writeTVar (ctx'nextIdVar ctx) $! childId + 1
  child <- do
    derivedCancelStateVar <- do
      derivedCancelState <-
        readTVar (ctx'cancelStateVar ctx) <&> \case
          CancelState'NotCancelled -> CancelState'NotCancelled
          CancelState'Cancelled token _cancelWay -> CancelState'Cancelled token CancelWay'Indirect
      newTVar derivedCancelState
    newCtxSTM_ derivedCancelStateVar (modifyTVar' (ctx'childrenVar ctx) (IntMap.delete childId))
  children <- readTVar (ctx'childrenVar ctx)
  writeTVar (ctx'childrenVar ctx) $! IntMap.insert childId child children
  pure child

cancelCtx :: Ctx -> CancelToken -> STM ()
cancelCtx ctx token =
  readTVar (ctx'cancelStateVar ctx) >>= \case
    CancelState'NotCancelled -> do
      cancelChildren ctx
      writeTVar (ctx'cancelStateVar ctx) $! CancelState'Cancelled token CancelWay'Direct
      ctx'removeFromParent ctx
    CancelState'Cancelled _token _way -> pure ()
  where
    cancelChildren :: Ctx -> STM ()
    cancelChildren Ctx {ctx'childrenVar} = do
      children <- readTVar ctx'childrenVar
      for_ (IntMap.elems children) \child -> do
        cancelChildren child
        writeTVar (ctx'cancelStateVar child) $! CancelState'Cancelled token CancelWay'Indirect
