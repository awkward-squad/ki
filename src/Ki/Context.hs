module Ki.Context
  ( Context (..),
    dummyContext,
    globalContext,
  )
where

import Ki.CancelToken (CancelToken)
import Ki.Ctx
import Ki.Prelude

data Context = Context
  { cancel :: IO (),
    cancelled :: STM CancelToken,
    -- | Derive a child context from a parent context.
    --
    --   * If the parent is already cancelled, so is the child.
    --   * If the parent isn't already canceled, the child registers itself with the
    --     parent such that:
    --       * When the parent is cancelled, so is the child
    --       * When the child is cancelled, it removes the parent's reference to it
    derive :: STM Context
  }

deriveContext :: Ctx -> Context
deriveContext ctx =
  Context
    { cancel = cancelCtx ctx,
      cancelled = ctxCancelToken ctx,
      derive = deriveContext <$> deriveCtx ctx
    }

dummyContext :: Context
dummyContext =
  Context
    { cancel = pure (),
      cancelled = retry,
      derive = pure dummyContext
    }

-- | The global context. It cannot be cancelled.
globalContext :: Context
globalContext =
  Context
    { cancel = pure (),
      cancelled = retry,
      derive = deriveContext <$> newCtxSTM
    }
