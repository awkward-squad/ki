module Ki.Internal.Context
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

import Ki.Internal.Context.Internal (CancelToken (..), Cancelled (..), background, cancel, cancelled, derive, new)
import qualified Ki.Internal.Context.Internal as Internal

type Context =
  ?context :: Internal.Context
