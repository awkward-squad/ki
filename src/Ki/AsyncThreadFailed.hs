module Ki.AsyncThreadFailed
  ( AsyncThreadFailed (..),
    unwrap,
  )
where

import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException)
import Ki.Prelude

-- | Internal exception type that is thrown from a thread to its parent.
newtype AsyncThreadFailed
  = AsyncThreadFailed SomeException
  deriving stock (Show)

instance Exception AsyncThreadFailed where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

unwrap :: SomeException -> SomeException
unwrap ex =
  case fromException ex of
    Just (AsyncThreadFailed exception) -> exception
    _ -> ex
