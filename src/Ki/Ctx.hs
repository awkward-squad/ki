{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Ctx
  ( Ctx (..),
    CancelState (..),
    CancelWay (..),
    newCtxSTM,
    deriveCtx,
    cancelCtx,
    cancelCtxSTM,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Ki.CancelToken
import Ki.Prelude

data Ctx = Ctx
  { cancelStateVar :: TVar CancelState,
    childrenVar :: TVar (IntMap Ctx),
    -- | The next id to assign to a child context. The child needs a unique identifier so it can delete itself from its
    -- parent's children map if it's cancelled independently. Wrap-around seems ok; that's a *lot* of children for one
    -- parent to have.
    nextIdVar :: TVar Int,
    -- | When I'm cancelled, this action removes myself from my parent's context. This isn't simply a pointer to the
    -- parent 'Ctx' for three reasons:
    --
    --   * "Root" contexts don't have a parent, so it'd have to be a Maybe (one more pointer indirection)
    --   * We don't really need a reference to the parent, because we only want to be able to remove ourselves from its
    --     children map, so just storing the STM action that does exactly seems a bit safer, even if conceptually it's
    --     a bit indirect.
    --   * If we stored a reference to the parent, we'd also have to store our own id, rather than just currying it into
    --     this action.
    onCancel :: STM ()
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
newCtxSTM_ cancelStateVar onCancel = do
  childrenVar <- newTVar IntMap.empty
  nextIdVar <- newTVar 0
  pure Ctx {cancelStateVar, childrenVar, nextIdVar, onCancel}

deriveCtx :: Ctx -> STM Ctx
deriveCtx Ctx {cancelStateVar, childrenVar, nextIdVar} = do
  childId <- readTVar nextIdVar
  writeTVar nextIdVar $! childId + 1
  child <- do
    derivedCancelStateVar <- do
      derivedCancelState <-
        readTVar cancelStateVar <&> \case
          CancelState'NotCancelled -> CancelState'NotCancelled
          CancelState'Cancelled token _cancelWay -> CancelState'Cancelled token CancelWay'Indirect
      newTVar derivedCancelState
    newCtxSTM_ derivedCancelStateVar (modifyTVar' childrenVar (IntMap.delete childId))
  children <- readTVar childrenVar
  writeTVar childrenVar $! IntMap.insert childId child children
  pure child

cancelCtx :: Ctx -> IO ()
cancelCtx context = do
  token <- newCancelToken
  atomically (cancelCtxSTM context token)

cancelCtxSTM :: Ctx -> CancelToken -> STM ()
cancelCtxSTM ctx token =
  readTVar (cancelStateVar ctx) >>= \case
    CancelState'NotCancelled -> do
      cancelChildren ctx
      writeTVar (cancelStateVar ctx) $! CancelState'Cancelled token CancelWay'Direct
      onCancel ctx
    CancelState'Cancelled _token _way -> pure ()
  where
    cancelChildren :: Ctx -> STM ()
    cancelChildren Ctx {childrenVar} = do
      children <- readTVar childrenVar
      for_ (IntMap.elems children) \child -> do
        cancelChildren child
        writeTVar (cancelStateVar child) $! CancelState'Cancelled token CancelWay'Indirect
