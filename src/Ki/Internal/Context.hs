module Ki.Internal.Context
  ( Context (..),
    CancelState (..),
    globalContext,
    newContext,
    contextCancelToken,
    deriveContext,
    cancelContext,
  )
where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Ki.Internal.CancelToken
import Ki.Internal.Prelude
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
  { context'cancelStateVar :: {-# UNPACK #-} !(TVar CancelState),
    context'childrenVar :: {-# UNPACK #-} !(TVar (Seq Context)),
    context'id :: {-# UNPACK #-} !Unique
  }

-- | The current cancel state.
--
-- @
-- +---------------+             +-----------+
-- | Not cancelled |-- cancel -->| Cancelled |
-- +---------------+             +-----------+
-- @
data CancelState
  = CancelState'NotCancelled
  | CancelState'Cancelled {-# UNPACK #-} !CancelToken

-- | The global context.
--
-- This should only be used at the top level of your program, wherever your reader monad is provided an initial
-- environment.
globalContext :: Context
globalContext =
  Context
    { context'cancelStateVar = unsafePerformIO (newTVarIO CancelState'NotCancelled),
      context'childrenVar = unsafePerformIO (newTVarIO Seq.empty),
      context'id = unsafePerformIO newUnique
    }
{-# NOINLINE globalContext #-}

newContext :: IO Context
newContext = do
  id_ <- newUnique
  atomically do
    cancelStateVar <- newTVar CancelState'NotCancelled
    newContextSTM cancelStateVar id_

newContextSTM :: TVar CancelState -> Unique -> STM Context
newContextSTM context'cancelStateVar context'id = do
  context'childrenVar <- newTVar Seq.empty
  pure Context {context'cancelStateVar, context'childrenVar, context'id}

contextCancelToken :: Context -> STM CancelToken
contextCancelToken context =
  readTVar (context'cancelStateVar context) >>= \case
    CancelState'NotCancelled -> retry
    CancelState'Cancelled token -> pure token

-- | Derive a child context from a parent context.
--
--   * If the parent is already cancelled, so is the child. In this case, we don't need to record the child as such (in
--     the parent's list of children), because the purpose of that list is only to propagate cancellation! (We therefore
--     can also just store a dummy, no-op "remove me from parent" inside the child context).
--   * If the parent isn't already canceled, the child registers itself with the parent, so cancellation can propagate
--     from parent to child.
deriveContext :: Context -> IO (Context, STM ())
deriveContext parent = do
  id_ <- newUnique
  atomically do
    readTVar (context'cancelStateVar parent) >>= \case
      CancelState'NotCancelled -> do
        childCancelStateVar <- newTVar CancelState'NotCancelled
        child <- newContextSTM childCancelStateVar id_
        modifyTVar' (context'childrenVar parent) (Seq.|> child)
        pure (child, removeChild id_)
      CancelState'Cancelled token -> do
        childCancelStateVar <- newTVar (CancelState'Cancelled token)
        child <- newContextSTM childCancelStateVar id_
        pure (child, pure ())
  where
    removeChild :: Unique -> STM ()
    removeChild id_ =
      modifyTVar' (context'childrenVar parent) \children ->
        case Seq.findIndexL (\child -> context'id child == id_) children of
          Nothing -> children -- should never happen, but eh.
          Just n -> Seq.deleteAt n children

-- | Cancel a context with the given token. Does nothing if the context is already cancelled.
cancelContext :: Context -> CancelToken -> STM ()
cancelContext context token =
  readTVar (context'cancelStateVar context) >>= \case
    CancelState'NotCancelled -> do
      writeTVar (context'cancelStateVar context) $! CancelState'Cancelled token
      cancelChildren context
    CancelState'Cancelled _token -> pure ()
  where
    cancelChildren :: Context -> STM ()
    cancelChildren Context {context'childrenVar} = do
      children <- readTVar context'childrenVar
      for_ children \child -> do
        writeTVar (context'cancelStateVar child) $! CancelState'Cancelled token
        cancelChildren child
