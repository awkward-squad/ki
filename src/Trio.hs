{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Trio
  ( withNursery
  , forkChild
  , Nursery
  , ChildDied(..)
  , Internal.NurseryClosed(..)
  ) where

import Control.Concurrent (ThreadId)
import Control.Exception (Exception, SomeException, catch, throwIO)

import qualified Trio.Internal as Internal


newtype Nursery
  = Nursery
  { unNursery :: Internal.Nursery IO }

data ChildDied
  = ChildDied
  { threadId :: ThreadId
  , exception :: SomeException
  } deriving stock ( Show )
    deriving anyclass ( Exception )

withNursery :: ( Nursery -> IO a ) -> IO a
withNursery f =
  catch
    ( Internal.withNursery \nursery -> f ( Nursery nursery ) )
    ( throwIO . translateChildDied )

translateChildDied :: Internal.ChildDied IO -> ChildDied
translateChildDied Internal.ChildDied{..} =
  ChildDied{..}

forkChild :: Nursery -> IO () -> IO ThreadId
forkChild nursery action =
  Internal.forkChild ( unNursery nursery ) action
