{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Ctx
  ( Ctx (..),
    newCtxSTM,
    deriveCtx,
    cancelCtx,
    cancelCtxSTM,
    ctxCancelToken,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Ki.CancelToken
import Ki.Prelude

data Ctx = Ctx
  { cancelTokenVar :: TVar (Maybe CancelToken),
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

newCtxSTM :: STM Ctx
newCtxSTM =
  newCtxSTM_ (pure ())

newCtxSTM_ :: STM () -> STM Ctx
newCtxSTM_ onCancel = do
  cancelTokenVar <- newTVar Nothing
  childrenVar <- newTVar IntMap.empty
  nextIdVar <- newTVar 0
  pure Ctx {cancelTokenVar, childrenVar, nextIdVar, onCancel}

deriveCtx :: Ctx -> STM Ctx
deriveCtx context@Ctx {cancelTokenVar, childrenVar, nextIdVar} =
  readTVar cancelTokenVar >>= \case
    Nothing -> do
      childId <- readTVar nextIdVar
      writeTVar nextIdVar $! childId + 1
      child <- newCtxSTM_ (modifyTVar' childrenVar (IntMap.delete childId))
      children <- readTVar childrenVar
      writeTVar childrenVar $! IntMap.insert childId child children
      pure child
    Just (CancelToken _) -> pure context

cancelCtx :: Ctx -> IO ()
cancelCtx context = do
  token <- newCancelToken
  atomically (cancelCtxSTM context token)

cancelCtxSTM :: Ctx -> CancelToken -> STM ()
cancelCtxSTM ctx@Ctx {onCancel} token =
  whenCanceling ctx token do
    cancelChildren ctx token
    onCancel

ctxCancelSTM_ :: CancelToken -> Ctx -> STM ()
ctxCancelSTM_ token ctx =
  whenCanceling ctx token (cancelChildren ctx token)

whenCanceling :: Ctx -> CancelToken -> STM () -> STM ()
whenCanceling Ctx {cancelTokenVar} token action =
  readTVar cancelTokenVar >>= \case
    Nothing -> do
      writeTVar cancelTokenVar $! Just token
      action
    Just (CancelToken _) -> pure ()

cancelChildren :: Ctx -> CancelToken -> STM ()
cancelChildren Ctx {childrenVar} token = do
  children <- readTVar childrenVar
  for_ (IntMap.elems children) (ctxCancelSTM_ token)

ctxCancelToken :: Ctx -> STM CancelToken
ctxCancelToken Ctx {cancelTokenVar} =
  readTVar cancelTokenVar >>= \case
    Nothing -> retry
    Just token -> pure token
