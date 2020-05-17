{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Ki.Indef.Context
  ( Context,
    background,
    derive,
    cancel,
  )
where

import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word32)
import Ki.Sig -- (STM, readTVar, TVar)

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

background :: Context
background =
  Background

derive :: Context -> STM Context
derive = \case
  Background -> pure Background
  Context contextVar -> Context <$> derive_ contextVar

derive_ :: TVar Ctx -> STM (TVar Ctx)
derive_ parentVar =
  readTVar parentVar >>= \case
    CtxOpen OpenCtx {nextId = childId, children, onCancel} -> do
      child <-
        newTVar
          "context"
          ( CtxOpen
              ( OpenCtx
                  { nextId = 0,
                    children = Map.empty,
                    onCancel = deleteChildFromParent childId
                  }
              )
          )
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
