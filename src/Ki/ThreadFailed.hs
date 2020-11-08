module Ki.ThreadFailed
  ( ThreadFailed (..),
    ThreadFailedAsync (..),
  )
where

import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException)
import Ki.Prelude

-- | A __thread__ failed, either by throwing or being thrown an exception.
data ThreadFailed = ThreadFailed
  { threadId :: ThreadId,
    exception :: SomeException
  }
  deriving stock (Show)
  deriving anyclass (Exception)

newtype ThreadFailedAsync
  = ThreadFailedAsync ThreadFailed
  deriving stock (Show)

instance Exception ThreadFailedAsync where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException
