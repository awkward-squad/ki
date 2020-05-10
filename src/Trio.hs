{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Trio
  ( withScope,
    async,
    asyncMasked,
    Scope,
    ChildDied (..),
    Internal.ScopeClosed (..),
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, catch, throwIO)
import qualified Trio.Internal as Internal

newtype Scope = Scope
  {unScope :: Internal.Scope IO}

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

withScope :: (Scope -> IO a) -> IO a
withScope f =
  catch
    (Internal.withScope \scope -> f (Scope scope))
    (throwIO . translateChildDied)

translateChildDied :: Internal.ChildDied IO -> ChildDied
translateChildDied Internal.ChildDied {..} =
  ChildDied {..}

async :: Scope -> IO a -> IO (Async a)
async scope action =
  asyncMasked scope \unmask -> unmask action

asyncMasked ::
  Scope ->
  ((forall x. IO x -> IO x) -> IO a) ->
  IO (Async a)
asyncMasked scope action =
  translateAsync <$> Internal.asyncMasked (unScope scope) action

translateAsync :: Internal.Async IO a -> Async a
translateAsync Internal.Async {..} =
  Async {..}
