module Ki.CancelToken
  ( CancelToken (..),
    newCancelToken,
  )
where

import Control.Exception
  ( Exception (fromException, toException),
    asyncExceptionFromException,
    asyncExceptionToException,
  )
import Ki.Prelude

-- | A __cancel token__ represents a request for /cancellation/; this request can be fulfilled by throwing the
-- __cancel token__ as an exception.
--
-- A __cancel token__ need not always be thrown as an exception. A __thread__ may instead prefer to return a value upon
-- observing that its __scope__ is /cancelled/.
--
-- A thrown __cancel token__ will stop propagating when it reaches the __scope__ in which the /cancellation/ request
-- originated.
--
-- Because it is always a mistake to manually stop a __cancel token__ from propagating, this is an asynchronous
-- exception type, even though it is never delivered to a __thread__ asynchronously by this library. The intention is to
-- comport with good exception-handling practices, which dictate that asynchronous exceptions should always be
-- re-thrown.
newtype CancelToken
  = CancelToken Unique
  deriving stock (Eq, Show)

instance Exception CancelToken where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

newCancelToken :: IO CancelToken
newCancelToken =
  coerce newUnique
