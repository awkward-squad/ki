{-# LANGUAGE PatternSynonyms #-}

module Ki.Context
  ( Context,
    dummy,
    derive,
    global,
    cancel,
    cancelled,
    unlessCancelled,
    matchCancelled,
    pattern Cancelled,
  )
where

import Ki.Concurrency
import Ki.Context.Internal (pattern Cancelled)
import qualified Ki.Context.Internal
import Ki.Prelude

data Context = Context
  { cancel :: IO (),
    cancelled :: forall a. STM (IO a),
    -- | Derive a child context from a parent context.
    --
    --   * If the parent is already cancelled, so is the child.
    --   * If the parent isn't already canceled, the child registers itself with the
    --     parent such that:
    --       * When the parent is cancelled, so is the child
    --       * When the child is cancelled, it removes the parent's reference to it
    derive :: STM Context,
    matchCancelled :: SomeException -> STM Bool
  }

dummy :: Context
dummy =
  Context
    { cancel = pure (),
      cancelled = retry,
      derive = pure dummy,
      matchCancelled = const (pure False)
    }

-- | The global context. It cannot be cancelled.
global :: Context
global =
  Context
    { cancel = pure (),
      cancelled = retry,
      derive = f <$> Ki.Context.Internal.newSTM,
      matchCancelled = const (pure False)
    }
  where
    f :: Ki.Context.Internal.Context -> Context
    f context =
      Context
        { cancel = Ki.Context.Internal.cancel context,
          cancelled = Ki.Context.Internal.cancelled context,
          derive = f <$> Ki.Context.Internal.derive context,
          matchCancelled = Ki.Context.Internal.matchCancelled context
        }

unlessCancelled :: Context -> IO a -> IO a
unlessCancelled context action =
  join (atomically (cancelled context <|> pure action))
