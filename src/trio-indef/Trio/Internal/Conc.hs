{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Random concurrency utils.
module Trio.Internal.Conc
  ( blockUntilTVar,
    registerBlock,
    retryingUntilSuccess,
  )
where

import Control.Exception (SomeException)
import Control.Monad (unless)
import Data.Function (fix)
import Trio.Sig (IO, STM, TVar, catch, readTVar, registerDelay, retry)
import Prelude hiding (IO)

blockUntilTVar :: TVar a -> (a -> Bool) -> STM ()
blockUntilTVar var f = do
  value <- readTVar var
  unless (f value) retry

registerBlock :: Int -> IO (STM ())
registerBlock micros = do
  delayVar <- registerDelay micros
  pure $
    readTVar delayVar >>= \case
      False -> retry
      True -> pure ()

-- | Execute an IO action until it successfully completes, ignoring all
-- synchronous and asynchronous exceptions.
retryingUntilSuccess :: IO a -> IO a
retryingUntilSuccess action =
  fix \again ->
    catch @SomeException action \_ -> again
