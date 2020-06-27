module Ki.Internal.Context
  ( -- * Context
    Context,
    dummy,
    new,
    derive,
    cancel,
    cancelled,
    CancelToken (..),
    Cancelled (..),
  )
where

import Ki.Internal.Context.Internal (CancelToken (..), Cancelled (..), cancel, cancelled, derive, dummy, new)
import qualified Ki.Internal.Context.Internal as Internal

type Context =
  ?context :: Internal.Context
