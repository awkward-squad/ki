{-# LANGUAGE StrictData #-}

module Ki.Indef.Context
  ( -- * Context
    Context,
    background,
    derive,

    -- * Cancellation
    cancel,
    CancelToken (..),
    cancelled,
    cancelledSTM,
    Cancelled (..),
  )
where

import qualified Data.Map as Map
import Ki.Internal.Concurrency
import Ki.Internal.Prelude

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ forked within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
data Context
  = Background
  | Context (TVar Ctx)
  deriving stock (Generic)

data Ctx
  = CtxOpen OpenCtx
  | CtxCancelled CancelToken

data OpenCtx = OpenCtx
  { -- | The next id to assign to a child context. The child needs a unique identifier so it can delete itself from our
    -- "cancel children" map if it's cancelled independently. Word wrap-around seems ok; that's a *lot* of contexts.
    nextId :: Word32,
    children :: Map Word32 (TVar Ctx),
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

background :: Context
background =
  Background

new :: STM () -> STM (TVar Ctx)
new onCancel =
  newTVar (CtxOpen OpenCtx {nextId = 0, children = Map.empty, onCancel})

-- | Derive a child context from a parent context.
--
--   * If the parent is already cancelled, so is the child.
--   * If the parent isn't already canceled, the child registers itself with the
--     parent such that:
--       * When the parent is cancelled, so is the child
--       * When the child is cancelled, it removes the parent's reference to it
derive :: Context -> STM Context
derive = \case
  Background -> Context <$> new (pure ())
  Context contextVar -> Context <$> derive_ contextVar

derive_ :: TVar Ctx -> STM (TVar Ctx)
derive_ parentVar =
  readTVar parentVar >>= \case
    CtxOpen OpenCtx {nextId = childId, children, onCancel} -> do
      child <- new (deleteChildFromParent childId)
      let children' = Map.insert childId child children
      writeTVar parentVar $! CtxOpen OpenCtx {nextId = childId + 1, children = children', onCancel}
      pure child
    CtxCancelled _ -> pure parentVar -- ok to reuse
  where
    deleteChildFromParent :: Word32 -> STM ()
    deleteChildFromParent childId =
      readTVar parentVar >>= \case
        CtxOpen ctx@OpenCtx {children} -> writeTVar parentVar $! CtxOpen ctx {children = Map.delete childId children}
        CtxCancelled _ -> pure ()

cancelled :: Context -> IO (Maybe CancelToken)
cancelled = \case
  Background -> pure Nothing
  Context contextVar -> atomically (cancelled_ contextVar)

cancelledSTM :: Context -> STM (Maybe CancelToken)
cancelledSTM = \case
  Background -> pure Nothing
  Context contextVar -> cancelled_ contextVar

cancelled_ :: TVar Ctx -> STM (Maybe CancelToken)
cancelled_ contextVar =
  readTVar contextVar <&> \case
    CtxOpen _ -> Nothing
    CtxCancelled token -> Just token

cancel :: Context -> CancelToken -> STM ()
cancel context token =
  case context of
    Background -> pure ()
    Context contextVar -> do
      readTVar contextVar >>= \case
        CtxOpen OpenCtx {children, onCancel} -> do
          writeTVar contextVar (CtxCancelled token)
          for_ (Map.elems children) (cancel_ token)
          onCancel
        CtxCancelled _ -> pure ()

cancel_ :: CancelToken -> TVar Ctx -> STM ()
cancel_ token contextVar =
  readTVar contextVar >>= \case
    CtxOpen OpenCtx {children} -> do
      writeTVar contextVar (CtxCancelled token)
      for_ (Map.elems children) (cancel_ token)
    CtxCancelled _ -> pure ()
