{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Trio
  ( withScope,
    close,
    async,
    asyncMasked,
    cancel,
    Scope,
    Async,
    ChildDied (..),
    Internal.ScopeClosed (..),
  )
where

import Control.Concurrent (ThreadId)
import Control.Exception (Exception, SomeException, catch, throwIO)
import Data.Coerce (coerce)
import qualified Trio.Internal as Internal

newtype Scope = Scope
  {unScope :: Internal.Scope IO}

newtype Async a
  = Async (Internal.Async IO a)

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

close :: Scope -> IO ()
close =
  coerce Internal.close

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
  coerce (Internal.asyncMasked (unScope scope) action)

cancel :: Async a -> IO ()
cancel =
  coerce Internal.cancel
