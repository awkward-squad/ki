{-# LANGUAGE StrictData #-}

module Ki.Internal.Context.Internal
  ( -- * Context
    Context,
    empty,
    derive,

    -- * Cancellation
    cancel,
    CancelToken (..),
    cancelled,
    Cancelled (..),
  )
where

import qualified Data.Map as Map
import Ki.Internal.Concurrency
import Ki.Internal.Prelude

data Context
  = Context OpenContext
  | ContextCancelled CancelToken

data OpenContext = OpenContext
  { -- | The next id to assign to a child context. The child needs a unique identifier so it can delete itself from our
    -- "cancel children" map if it's cancelled independently. Word wrap-around seems ok; that's a *lot* of contexts.
    nextId :: Word32,
    children :: Map Word32 (TVar Context),
    onCancel :: STM ()
  }

newtype CancelToken
  = CancelToken Int
  deriving stock (Eq, Show)

-- | A 'Cancelled' exception is thrown when a __thread__ voluntarily capitulates after observing its __context__ is
-- /cancelled/.
data Cancelled
  = Cancelled_ CancelToken
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

empty :: STM (TVar Context)
empty =
  newTVar
    ( Context
        OpenContext
          { nextId = 0,
            children = Map.empty,
            onCancel = pure ()
          }
    )

derive :: TVar Context -> STM (TVar Context)
derive parentVar =
  readTVar parentVar >>= \case
    Context OpenContext {nextId = childId, children, onCancel} -> do
      child <-
        newTVar
          ( Context
              OpenContext
                { nextId = 0,
                  children = Map.empty,
                  onCancel = deleteChildFromParent childId
                }
          )
      let children' = Map.insert childId child children
      writeTVar parentVar $! Context OpenContext {nextId = childId + 1, children = children', onCancel}
      pure child
    ContextCancelled _ -> pure parentVar -- ok to reuse
  where
    deleteChildFromParent :: Word32 -> STM ()
    deleteChildFromParent childId =
      readTVar parentVar >>= \case
        Context ctx@OpenContext {children} ->
          writeTVar parentVar $! Context ctx {children = Map.delete childId children}
        ContextCancelled _ -> pure ()

cancelled :: TVar Context -> STM (Maybe CancelToken)
cancelled contextVar =
  readTVar contextVar <&> \case
    Context _ -> Nothing
    ContextCancelled token -> Just token

cancel :: TVar Context -> CancelToken -> STM ()
cancel contextVar token =
  readTVar contextVar >>= \case
    Context OpenContext {children, onCancel} -> do
      writeTVar contextVar (ContextCancelled token)
      for_ (Map.elems children) (cancel_ token)
      onCancel
    ContextCancelled _ -> pure ()

cancel_ :: CancelToken -> TVar Context -> STM ()
cancel_ token contextVar =
  readTVar contextVar >>= \case
    Context OpenContext {children} -> do
      writeTVar contextVar (ContextCancelled token)
      for_ (Map.elems children) (cancel_ token)
    ContextCancelled _ -> pure ()
