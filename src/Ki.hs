{-# LANGUAGE PatternSynonyms #-}

module Ki
  ( -- * Context
    K.Context,
    K.global,
    K.cancelled,
    K.cancelledSTM,

    -- * Scope
    K.Scope,
    K.scoped,
    K.wait,
    K.waitSTM,
    K.waitFor,
    K.cancel,

    -- * Thread
    K.Thread,
    K.async,
    K.asyncWithUnmask,
    K.fork,
    K.forkWithUnmask,
    K.await,
    K.awaitSTM,
    K.awaitFor,
    K.kill,

    -- * Exceptions
    K.Cancelled (Cancelled),

    -- * Miscellaneous
    K.Seconds,
    K.timeout,
  )
where

import qualified Ki.Indef as K

-- | A 'Cancelled' exception is thrown when a __thread__ voluntarily capitulates after observing its __context__ is
-- /cancelled/.
pattern Cancelled :: K.Cancelled
pattern Cancelled <- K.Cancelled_ _

{-# COMPLETE Cancelled #-}
