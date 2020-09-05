module Ki.Context
  ( Context (..),
    deriveCtx,
    dummy,
    global,
  )
where

import Ki.CancelToken (CancelToken)
import Ki.Context.Internal
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

deriveCtx :: Ctx -> Context
deriveCtx ctx =
  Context
    { cancel = ctxCancel ctx,
      cancelled = ctxCancelled ctx,
      derive = deriveCtx <$> ctxDerive ctx
    }

dummy :: Context
dummy =
  Context
    { cancel = pure (),
      cancelled = retry,
      derive = pure dummy
    }

-- | The global context. It cannot be cancelled.
global :: Context
global =
  Context
    { cancel = pure (),
      cancelled = retry,
      derive = deriveCtx <$> ctxNewSTM
    }
