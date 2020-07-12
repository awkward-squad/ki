{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Context.Internal
  ( -- * Context
    Context,
    Cancelled (..),
    CancelToken,
    newSTM,
    derive,
    cancel,
    cancelled,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Ki.Concurrency
import Ki.Prelude

data Context = Context
  { cancelTokenVar :: TVar (Maybe CancelToken),
    childrenVar :: TVar (IntMap Context),
    -- | The next id to assign to a child context. The child needs a unique identifier so it can delete itself from its
    -- parent's children map if it's cancelled independently. Wrap-around seems ok; that's a *lot* of children for one
    -- parent to have.
    nextIdVar :: TVar Int,
    -- | When I'm cancelled, this action removes myself from my parent's context. This isn't simply a pointer to the
    -- parent (i.e. Context_) for three reasons:
    --
    --   * "Root" contexts don't have a parent, so it'd have to be a Maybe (one more pointer indirection)
    --   * We don't really need a reference to the parent, because we only want to be able to remove ourselves from its
    --     children map, so just storing the STM action that does exactly seems a bit safer, even if conceptually it's
    --     a bit indirect.
    --   * If we stored a reference to the parent, we'd also have to store our own id, rather than just currying it into
    --     this action.
    onCancel :: STM ()
  }

-- | A 'Cancelled' exception is thrown when a __thread__ voluntarily capitulates after observing its __context__ is
-- /cancelled/.
newtype Cancelled
  = Cancelled CancelToken
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

newtype CancelToken
  = CancelToken Int
  deriving stock (Eq, Show)

newSTM :: STM Context
newSTM =
  newWith (pure ())

newWith :: STM () -> STM Context
newWith onCancel = do
  cancelTokenVar <- newTVar Nothing
  childrenVar <- newTVar IntMap.empty
  nextIdVar <- newTVar 0
  pure Context {cancelTokenVar, childrenVar, nextIdVar, onCancel}

derive :: Context -> STM Context
derive context@Context {cancelTokenVar, childrenVar, nextIdVar} =
  readTVar cancelTokenVar >>= \case
    Nothing -> do
      childId <- readTVar nextIdVar
      writeTVar nextIdVar $! childId + 1
      child <- newWith (modifyTVar' childrenVar (IntMap.delete childId))
      children <- readTVar childrenVar
      writeTVar childrenVar $! IntMap.insert childId child children
      pure child
    Just _ -> pure context

cancel :: Context -> IO ()
cancel context = do
  token <- uniqueInt
  atomically (cancelSTM context (CancelToken token))

cancelSTM :: Context -> CancelToken -> STM ()
cancelSTM Context {cancelTokenVar, childrenVar, onCancel} token =
  readTVar cancelTokenVar >>= \case
    Nothing -> do
      writeTVar cancelTokenVar $! Just token
      children <- readTVar childrenVar
      for_ (IntMap.elems children) (cancelSTM_ token)
      onCancel
    Just _ -> pure ()

cancelSTM_ :: CancelToken -> Context -> STM ()
cancelSTM_ token Context {cancelTokenVar, childrenVar} =
  readTVar cancelTokenVar >>= \case
    Nothing -> do
      writeTVar cancelTokenVar $! Just token
      children <- readTVar childrenVar
      for_ (IntMap.elems children) (cancelSTM_ token)
    Just _ -> pure ()

cancelled :: Context -> STM CancelToken
cancelled Context {cancelTokenVar} =
  readTVar cancelTokenVar >>= \case
    Nothing -> retry
    Just token -> pure token
