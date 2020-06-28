{-# LANGUAGE PatternSynonyms #-}

module Ki.Context.Internal
  ( -- * Context
    Context,
    dummy,
    new,
    derive,
    cancel,
    cancelled,
    matchCancelled,
    pattern Cancelled,
  )
where

import Ki.Concurrency
import Ki.Context.Internal.Internal (pattern Cancelled)
import qualified Ki.Context.Internal.Internal as Internal
import Ki.Prelude

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ forked within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
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

-- | Create a new context without a parent.
new :: IO Context
new =
  f <$> Internal.new
  where
    f :: Internal.Context -> Context
    f context =
      Context
        { cancel = Internal.cancel context,
          cancelled = Internal.cancelled context,
          derive = f <$> Internal.derive context,
          matchCancelled = Internal.matchCancelled context
        }
