module Ki.Context
  ( Context (..),
    dummyContext,
    globalContext,
    newContext,
    contextCancelToken,
  )
where

import Ki.CancelToken (CancelToken)
import Ki.Ctx
import Ki.Prelude

-- Note: keep this haddock up-to-date with Ki.Implicit.Context

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ created within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
data Context = Context
  { context'cancel :: IO (),
    -- | Get this content's current cancel state. This action never retries.
    context'cancelState :: STM CancelState,
    -- | Derive a child context from a parent context.
    --
    --   * If the parent is already cancelled, so is the child.
    --   * If the parent isn't already canceled, the child registers itself with the
    --     parent such that:
    --       * When the parent is cancelled, so is the child
    --       * When the child is cancelled, it removes the parent's reference to it
    context'derive :: STM Context
  }

newContext :: Ctx -> Context
newContext ctx =
  Context
    { context'cancel = cancelCtx ctx,
      context'cancelState = readTVar (ctx'cancelStateVar ctx),
      context'derive = newContext <$> deriveCtx ctx
    }

dummyContext :: Context
dummyContext =
  Context
    { context'cancel = pure (),
      context'cancelState = retry,
      context'derive = pure dummyContext
    }

-- | The global context. It cannot be cancelled.
globalContext :: Context
globalContext =
  Context
    { context'cancel = pure (),
      context'cancelState = pure CancelState'NotCancelled,
      context'derive = newContext <$> newCtxSTM
    }

contextCancelToken :: Context -> STM CancelToken
contextCancelToken context =
  context'cancelState context >>= \case
    CancelState'NotCancelled -> retry
    CancelState'Cancelled token _way -> pure token
