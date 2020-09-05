{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Context.Internal
  ( Ctx (..),
    ctxNewSTM,
    ctxDerive,
    ctxCancel,
    ctxCancelSTM,
    ctxCancelled,
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

ctxNewSTM :: STM Ctx
ctxNewSTM =
  newWith (pure ())

newWith :: STM () -> STM Ctx
newWith onCancel = do
  cancelTokenVar <- newTVar Nothing
  childrenVar <- newTVar IntMap.empty
  nextIdVar <- newTVar 0
  pure Ctx {cancelTokenVar, childrenVar, nextIdVar, onCancel}

ctxDerive :: Ctx -> STM Ctx
ctxDerive context@Ctx {cancelTokenVar, childrenVar, nextIdVar} =
  readTVar cancelTokenVar >>= \case
    Nothing -> do
      childId <- readTVar nextIdVar
      writeTVar nextIdVar $! childId + 1
      child <- newWith (modifyTVar' childrenVar (IntMap.delete childId))
      children <- readTVar childrenVar
      writeTVar childrenVar $! IntMap.insert childId child children
      pure child
    Just _ -> pure context

ctxCancel :: Ctx -> IO ()
ctxCancel context = do
  token <- newCancelToken
  atomically (ctxCancelSTM context token)

ctxCancelSTM :: Ctx -> CancelToken -> STM ()
ctxCancelSTM Ctx {cancelTokenVar, childrenVar, onCancel} token =
  readTVar cancelTokenVar >>= \case
    Nothing -> do
      writeTVar cancelTokenVar $! Just token
      children <- readTVar childrenVar
      for_ (IntMap.elems children) (cancelSTM_ token)
      onCancel
    Just _ -> pure ()

cancelSTM_ :: CancelToken -> Ctx -> STM ()
cancelSTM_ token Ctx {cancelTokenVar, childrenVar} =
  readTVar cancelTokenVar >>= \case
    Nothing -> do
      writeTVar cancelTokenVar $! Just token
      children <- readTVar childrenVar
      for_ (IntMap.elems children) (cancelSTM_ token)
    Just _ -> pure ()

ctxCancelled :: Ctx -> STM CancelToken
ctxCancelled Ctx {cancelTokenVar} =
  readTVar cancelTokenVar >>= \case
    Nothing -> retry
    Just token -> pure token
