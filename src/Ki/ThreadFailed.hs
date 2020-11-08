module Ki.ThreadFailed
  ( ThreadFailed (..),
    ThreadFailedAsync (..),
  )
where

import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException)
import Ki.Prelude

data ThreadFailed
  = ThreadFailed ThreadId SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

newtype ThreadFailedAsync
  = ThreadFailedAsync ThreadFailed
  deriving stock (Show)

instance Exception ThreadFailedAsync where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException
