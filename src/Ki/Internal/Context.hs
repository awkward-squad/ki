module Ki.Internal.Context
  ( -- Context (..),
    CancelState (..),
    -- globalContext,
    -- newContext,
    -- contextCancelToken,
  )
where

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
    -- | The list of derived contexts to which we must propagate cancellation.
    context'childrenVar :: {-# UNPACK #-} !(TVar (Seq Context)),
    -- | The globally unique identifier for this context.
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
_globalContext :: Context
_globalContext =
  unsafePerformIO do
    context'cancelStateVar <- newTVarIO CancelState'NotCancelled
    context'childrenVar <- newTVarIO Seq.empty
    context'id <- newUnique
    pure Context {context'cancelStateVar, context'childrenVar, context'id}
{-# NOINLINE _globalContext #-}

_newContext :: IO Context
_newContext = do
  id_ <- newUnique
  atomically do
    cancelStateVar <- newTVar CancelState'NotCancelled
    newContextSTM cancelStateVar id_

newContextSTM :: TVar CancelState -> Unique -> STM Context
newContextSTM context'cancelStateVar context'id = do
  context'childrenVar <- newTVar Seq.empty
  pure Context {context'cancelStateVar, context'childrenVar, context'id}

_contextCancelToken :: Context -> STM CancelToken
_contextCancelToken context =
  readTVar (context'cancelStateVar context) >>= \case
    CancelState'NotCancelled -> retry
    CancelState'Cancelled token -> pure token
