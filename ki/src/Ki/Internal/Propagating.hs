module Ki.Internal.Propagating
  ( pattern PropagatingFrom,
    Tid,
    peelOffPropagating,
    propagate,
  )
where

import Control.Concurrent (ThreadId)
import Control.Exception (Exception (..), SomeException, asyncExceptionFromException, asyncExceptionToException, throwTo)

-- Internal exception type thrown by a child thread to its parent, if it fails unexpectedly.
data Propagating = Propagating
  { childId :: {-# UNPACK #-} !Tid,
    exception :: !SomeException
  }
  deriving stock (Show)

instance Exception Propagating where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

pattern PropagatingFrom :: Tid -> SomeException
pattern PropagatingFrom childId <- (fromException -> Just Propagating {childId})

pattern PropagatingThe :: SomeException -> SomeException
pattern PropagatingThe exception <- (fromException -> Just Propagating {exception})

-- A unique identifier for a thread within a scope. (Internal type alias)
type Tid =
  Int

-- Peel an outer Propagating layer off of some exception, if there is one.
peelOffPropagating :: SomeException -> SomeException
peelOffPropagating = \case
  PropagatingThe exception -> exception
  exception -> exception

-- @propagate exception child parent@ propagates @exception@ from @child@ to @parent@.
propagate :: SomeException -> Tid -> ThreadId -> IO ()
propagate exception childId parentThreadId =
  throwTo parentThreadId Propagating {childId, exception}
