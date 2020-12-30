module Ki.Context
  ( Context (..),
    CancelState (..),
    CancelWay (..),
    globalContext,
    newContext,
    contextCancelToken,
    deriveContext,
    cancelContext,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Ki.CancelToken
import Ki.Prelude
import System.IO.Unsafe (unsafePerformIO)

-- Note: keep this haddock up-to-date with Ki.Implicit.Context

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ created within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
data Context = Context
  { -- | Get this content's current cancel state. This action never retries.
    context'cancelStateVar :: TVar CancelState,
    context'childrenVar :: TVar (IntMap Context),
    -- | The next id to assign to a child context. The child needs a unique identifier so it can delete itself from its
    -- parent's children map if it's cancelled independently. Wrap-around seems ok; that's a *lot* of children for one
    -- parent to have.
    context'nextIdVar :: TVar Int,
    -- | Remove myself from my parent's context. This isn't simply a pointer to the parent context for three reasons:
    --
    --   * "Root" contexts don't have a parent, so it'd have to be a Maybe (one more pointer indirection)
    --   * We don't really need a reference to the parent, because we only want to be able to remove ourselves from its
    --     children map, so just storing the STM action that does exactly seems a bit safer, even if conceptually it's
    --     a bit indirect.
    --   * If we stored a reference to the parent, we'd also have to store our own id, rather than just currying it into
    --     this action.
    context'removeFromParent :: STM ()
  }

data CancelState
  = CancelState'NotCancelled
  | CancelState'Cancelled CancelToken CancelWay

data CancelWay
  = CancelWay'Indirect -- parent (or grandparent, etc...) was cancelled
  | CancelWay'Direct -- we were cancelled

-- | The global context.
globalContext :: Context
globalContext =
  Context
    { context'cancelStateVar = unsafePerformIO (newTVarIO CancelState'NotCancelled),
      context'childrenVar = unsafePerformIO (newTVarIO IntMap.empty),
      context'nextIdVar = unsafePerformIO (newTVarIO 0),
      context'removeFromParent = pure ()
    }

newContext :: IO Context
newContext =
  atomically do
    cancelStateVar <- newTVar CancelState'NotCancelled
    newContextSTM cancelStateVar (pure ())

newContextSTM :: TVar CancelState -> STM () -> STM Context
newContextSTM context'cancelStateVar context'removeFromParent = do
  context'childrenVar <- newTVar IntMap.empty
  context'nextIdVar <- newTVar 0
  pure Context {context'cancelStateVar, context'childrenVar, context'nextIdVar, context'removeFromParent}

contextCancelToken :: Context -> STM CancelToken
contextCancelToken context =
  readTVar (context'cancelStateVar context) >>= \case
    CancelState'NotCancelled -> retry
    CancelState'Cancelled token _way -> pure token

-- | Derive a child context from a parent context.
--
--   * If the parent is already cancelled, so is the child.
--   * If the parent isn't already canceled, the child registers itself with the
--     parent such that:
--       * When the parent is cancelled, so is the child
--       * When the child is cancelled, it removes the parent's reference to it
deriveContext :: Context -> IO Context
deriveContext context =
  atomically do
    childId <- readTVar (context'nextIdVar context)
    writeTVar (context'nextIdVar context) $! childId + 1
    child <- do
      derivedCancelStateVar <- do
        derivedCancelState <-
          readTVar (context'cancelStateVar context) <&> \case
            CancelState'NotCancelled -> CancelState'NotCancelled
            CancelState'Cancelled token _cancelWay -> CancelState'Cancelled token CancelWay'Indirect
        newTVar derivedCancelState
      newContextSTM derivedCancelStateVar (modifyTVar' (context'childrenVar context) (IntMap.delete childId))
    children <- readTVar (context'childrenVar context)
    writeTVar (context'childrenVar context) $! IntMap.insert childId child children
    pure child

cancelContext :: Context -> CancelToken -> STM ()
cancelContext context token =
  readTVar (context'cancelStateVar context) >>= \case
    CancelState'NotCancelled -> do
      cancelChildren context
      writeTVar (context'cancelStateVar context) $! CancelState'Cancelled token CancelWay'Direct
      context'removeFromParent context
    CancelState'Cancelled _token _way -> pure ()
  where
    cancelChildren :: Context -> STM ()
    cancelChildren Context {context'childrenVar} = do
      children <- readTVar context'childrenVar
      for_ (IntMap.elems children) \child -> do
        cancelChildren child
        writeTVar (context'cancelStateVar child) $! CancelState'Cancelled token CancelWay'Indirect
