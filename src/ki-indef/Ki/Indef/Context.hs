{-# LANGUAGE StrictData #-}

module Ki.Indef.Context
  ( Context,
    background,
    cancelled,
    cancelledSTM,

    -- * Internal API
    derive,
    cancel,
  )
where

import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word32)
import Ki.Sig -- (STM, readTVar, TVar)
import Prelude hiding (IO)

-- | A __context__ models a program's call tree.
--
-- Every __thread__ has its own __context__, which is used as a mechanism to
-- propagate /cancellation/.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is
-- a suggestion to perform a graceful shutdown and finish.
data Context
  = Background
  | Context (TVar Ctx)

data Ctx
  = CtxOpen OpenCtx
  | CtxCancelled

data OpenCtx = OpenCtx
  { -- | The next id to assign to a child context. The child needs a unique
    -- identifier so it can delete itself from our "cancel children" map if it's
    -- cancelled independently. Word wrap-around seems ok; that's a *lot* of
    -- contexts.
    nextId :: Word32,
    children :: Map Word32 (TVar Ctx),
    onCancel :: STM ()
  }

-- | The background __context__.
--
-- You should only use this when another __context__ isn't available, as when
-- creating a top-level __scope__ from the main thread.
--
-- The background __context__ cannot be /cancelled/.
background :: Context
background =
  Background

-- | Return whether a __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ will be killed soon; they
-- should attempt to perform a graceful shutdown and finish.
cancelled :: Context -> IO Bool
cancelled = \case
  Background -> pure False
  Context contextVar -> atomically (cancelled_ contextVar)

-- | @STM@ variant of 'cancelled'.
cancelledSTM :: Context -> STM Bool
cancelledSTM = \case
  Background -> pure False
  Context contextVar -> cancelled_ contextVar

cancelled_ :: TVar Ctx -> STM Bool
cancelled_ contextVar =
  readTVar contextVar <&> \case
    CtxOpen _ -> False
    CtxCancelled -> True

new :: STM () -> STM (TVar Ctx)
new onCancel =
  newTVar "context" context
  where
    context :: Ctx
    context =
      CtxOpen
        OpenCtx
          { nextId = 0,
            children = Map.empty,
            onCancel
          }

-- Derive a child context from a parent context.
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
      writeTVar parentVar
        $! CtxOpen
          OpenCtx
            { nextId = childId + 1,
              children = Map.insert childId child children,
              onCancel
            }
      pure child
    CtxCancelled -> pure parentVar -- ok to reuse
  where
    deleteChildFromParent :: Word32 -> STM ()
    deleteChildFromParent childId =
      readTVar parentVar >>= \case
        CtxOpen ctx@OpenCtx {children} ->
          writeTVar parentVar
            $! CtxOpen
              ctx
                { children =
                    Map.delete childId children
                }
        CtxCancelled -> pure ()

cancel :: Context -> STM ()
cancel = \case
  Background -> pure ()
  Context contextVar -> do
    readTVar contextVar >>= \case
      CtxOpen OpenCtx {children, onCancel} -> do
        for_ (Map.elems children) cancel_
        writeTVar contextVar CtxCancelled
        onCancel
      CtxCancelled -> pure ()

cancel_ :: TVar Ctx -> STM ()
cancel_ contextVar =
  readTVar contextVar >>= \case
    CtxOpen OpenCtx {children} -> do
      for_ (Map.elems children) cancel_
      writeTVar contextVar CtxCancelled
    CtxCancelled -> pure ()
