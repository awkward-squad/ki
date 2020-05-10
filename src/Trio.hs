{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Trio
  ( withNursery,
    async,
    asyncMasked,
    Nursery,
    ChildDied (..),
    Internal.NurseryClosed (..),
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, catch, throwIO)
import qualified Trio.Internal as Internal

newtype Nursery = Nursery
  {unNursery :: Internal.Nursery IO}

data Async a = Async
  { threadId :: ThreadId,
    action :: STM (Either SomeException a)
  }

data ChildDied = ChildDied
  { threadId :: ThreadId,
    exception :: SomeException
  }
  deriving stock (Show)
  deriving anyclass (Exception)

withNursery :: (Nursery -> IO a) -> IO a
withNursery f =
  catch
    (Internal.withNursery \nursery -> f (Nursery nursery))
    (throwIO . translateChildDied)

translateChildDied :: Internal.ChildDied IO -> ChildDied
translateChildDied Internal.ChildDied {..} =
  ChildDied {..}

async :: Nursery -> IO a -> IO (Async a)
async nursery action =
  asyncMasked nursery \unmask -> unmask action

asyncMasked ::
  Nursery ->
  ((forall x. IO x -> IO x) -> IO a) ->
  IO (Async a)
asyncMasked nursery action =
  translateAsync <$> Internal.asyncMasked (unNursery nursery) action

translateAsync :: Internal.Async IO a -> Async a
translateAsync Internal.Async {..} =
  Async {..}
