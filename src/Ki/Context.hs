module Ki.Context
  ( Context (..),
    dummyContext,
    globalContext,
    newContext,
    contextCancelTokenSTM,
  )
where

import Ki.CancelToken (CancelToken)
import Ki.Ctx (CancelState, Ctx)
import qualified Ki.Ctx as Ctx
import Ki.Prelude

data Context = Context
  { cancelContext :: IO (),
    -- | Get this content's current cancel state. This action never retries.
    contextCancelStateSTM :: STM CancelState,
    -- | Derive a child context from a parent context.
    --
    --   * If the parent is already cancelled, so is the child.
    --   * If the parent isn't already canceled, the child registers itself with the
    --     parent such that:
    --       * When the parent is cancelled, so is the child
    --       * When the child is cancelled, it removes the parent's reference to it
    deriveContext :: STM Context
  }

newContext :: Ctx -> Context
newContext ctx =
  Context
    { cancelContext = Ctx.cancelCtx ctx,
      contextCancelStateSTM = readTVar (Ctx.cancelStateVar ctx),
      deriveContext = newContext <$> Ctx.deriveCtx ctx
    }

dummyContext :: Context
dummyContext =
  Context
    { cancelContext = pure (),
      contextCancelStateSTM = retry,
      deriveContext = pure dummyContext
    }

-- | The global context. It cannot be cancelled.
globalContext :: Context
globalContext =
  Context
    { cancelContext = pure (),
      contextCancelStateSTM = pure Ctx.CancelState'NotCancelled,
      deriveContext = newContext <$> Ctx.newCtxSTM
    }

contextCancelTokenSTM :: Context -> STM CancelToken
contextCancelTokenSTM Context {contextCancelStateSTM} =
  contextCancelStateSTM >>= \case
    Ctx.CancelState'NotCancelled -> retry
    Ctx.CancelState'Cancelled token _way -> pure token
