{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Trio
  ( withNursery
  , forkChild
  , forkMaskedChild
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
  Internal.forkMaskedChild ( unNursery nursery ) \unmask -> unmask action

forkMaskedChild
  :: Nursery
  -> ( ( forall x. IO x -> IO x ) -> IO () )
  -> IO ThreadId
forkMaskedChild nursery action =
  Internal.forkMaskedChild ( unNursery nursery ) action
