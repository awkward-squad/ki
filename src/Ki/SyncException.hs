module Ki.SyncException
  ( SyncException (..),
    throw,
  )
where

import Control.Exception (Exception (displayException, fromException), SomeAsyncException (..))
import Ki.Prelude

newtype SyncException = SyncException
  {unSyncException :: SomeException}

instance Exception SyncException where
  displayException (SyncException e) =
    displayException e

instance Show SyncException where
  show (SyncException e) =
    show e

throw :: SomeException -> IO a
throw e =
  case fromException e of
    Nothing -> throwIO e
    Just SomeAsyncException {} -> throwIO (SyncException e)
