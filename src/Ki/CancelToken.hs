module Ki.CancelToken
  ( CancelToken (..),
    newCancelToken,
  )
where

import Ki.Prelude

-- | A cancel token represents a request for /cancellation/; this request can be fulfilled by throwing the token as an
-- exception.
newtype CancelToken
  = CancelToken Int
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

newCancelToken :: IO CancelToken
newCancelToken =
  coerce uniqueInt
