{-# LANGUAGE TypeApplications #-}

module Ki.Internal.Context.Internal
  ( -- * Context
    Context,
    new,
    derive,
    cancel,
    cancelled,
    CancelToken (..),
    Cancelled (..),
  )
where

import qualified Data.Map as Map
import Ki.Internal.Concurrency
import Ki.Internal.Prelude

newtype Context
  = Context (TVar E)

data E
  = L CancelToken
  | R Context_

data Context_ = Context_
  { -- | The next id to assign to a child context. The child needs a unique identifier so it can delete itself from our
    -- "cancel children" map if it's cancelled independently. Word wrap-around seems ok; that's a *lot* of contexts.
    nextId :: Word32,
    children :: Map Word32 Context,
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

new :: STM Context
new =
  newWith (pure ())

newWith :: STM () -> STM Context
newWith onCancel =
  coerce @(STM (TVar E)) (newTVar (R (Context_ {nextId = 0, children = Map.empty, onCancel})))

derive :: Context -> STM Context
derive (Context parentVar) =
  readTVar parentVar >>= \case
    R ctx@Context_ {nextId, children} -> do
      child <- newWith (deleteChildFromParent nextId)
      writeTVar parentVar $! R ctx {nextId = nextId + 1, children = Map.insert nextId child children}
      pure child
    L _ -> pure (Context parentVar) -- ok to reuse
  where
    deleteChildFromParent :: Word32 -> STM ()
    deleteChildFromParent childId =
      readTVar parentVar >>= \case
        R ctx@Context_ {children} -> writeTVar parentVar $! R ctx {children = Map.delete childId children}
        L _ -> pure ()

cancel :: Context -> CancelToken -> STM ()
cancel (Context contextVar) token =
  readTVar contextVar >>= \case
    R Context_ {children, onCancel} -> do
      writeTVar contextVar (L token)
      for_ (Map.elems children) (cancel_ token)
      onCancel
    L _ -> pure ()

cancel_ :: CancelToken -> Context -> STM ()
cancel_ token (Context contextVar) =
  readTVar contextVar >>= \case
    R Context_ {children} -> do
      writeTVar contextVar (L token)
      for_ (Map.elems children) (cancel_ token)
    L _ -> pure ()

cancelled :: Context -> STM (Maybe CancelToken)
cancelled (Context contextVar) =
  readTVar contextVar <&> \case
    R _ -> Nothing
    L token -> Just token
