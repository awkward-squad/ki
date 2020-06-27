module Ki.Internal.Context.Internal
  ( -- * Context
    Context,
    background,
    new,
    derive,
    cancel,
    cancelled,
    CancelToken (..),
    Cancelled (..),
  )
where

import Ki.Internal.Concurrency
import Ki.Internal.Context.Internal.Internal (CancelToken (..), Cancelled (..))
import qualified Ki.Internal.Context.Internal.Internal as Internal
import Ki.Internal.Prelude

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ forked within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
data Context = Context
  { cancel :: CancelToken -> STM (),
    cancelled :: STM (Maybe CancelToken),
    -- | Derive a child context from a parent context.
    --
    --   * If the parent is already cancelled, so is the child.
    --   * If the parent isn't already canceled, the child registers itself with the
    --     parent such that:
    --       * When the parent is cancelled, so is the child
    --       * When the child is cancelled, it removes the parent's reference to it
    derive :: STM Context
  }
  deriving stock (Generic)

background :: Context
background =
  Context
    { cancel = const (pure ()),
      cancelled = pure Nothing,
      derive = pure background
    }

-- | Create a new context without a parent.
new :: STM Context
new =
  f <$> Internal.new
  where
    f :: Internal.Context -> Context
    f context =
      Context
        { cancel = Internal.cancel context,
          cancelled = Internal.cancelled context,
          derive = f <$> Internal.derive context
        }
