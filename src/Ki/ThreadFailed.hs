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

-- | An async wrapper around 'ThreadFailed', used when a child __thread__ communicates its failure to its parent. This
-- is preferred to throwing 'ThreadFailed' directly, so that client code (outside of this library) can follow
-- best-practices when encountering a mysterious async exception: clean up resources and re-throw it.
newtype ThreadFailedAsync
  = ThreadFailedAsync ThreadFailed
  deriving stock (Show)

instance Exception ThreadFailedAsync where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException
