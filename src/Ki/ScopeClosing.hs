module Ki.ScopeClosing
  ( ScopeClosing (..),
  )
where

import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException)
import Ki.Prelude

-- | Exception thrown by a parent __thread__ to its children when the __scope__ is closing.
data ScopeClosing
  = ScopeClosing
  deriving stock (Eq, Show)

instance Exception ScopeClosing where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException
