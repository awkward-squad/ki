module Ki.Context
  ( Context (..),
    dummyContext,
    globalContext,
  )
where

import Ki.CancelToken (CancelToken)
import Ki.Ctx (Ctx)
import qualified Ki.Ctx as Ctx
import Ki.Prelude

data Context = Context
  { cancelContext :: IO (),
    contextCancelTokenSTM :: STM CancelToken,
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
      contextCancelTokenSTM = Ctx.ctxCancelToken ctx,
      deriveContext = newContext <$> Ctx.deriveCtx ctx
    }

dummyContext :: Context
dummyContext =
  Context
    { cancelContext = pure (),
      contextCancelTokenSTM = retry,
      deriveContext = pure dummyContext
    }

-- | The global context. It cannot be cancelled.
globalContext :: Context
globalContext =
  Context
    { cancelContext = pure (),
      contextCancelTokenSTM = retry,
      deriveContext = newContext <$> Ctx.newCtxSTM
    }
