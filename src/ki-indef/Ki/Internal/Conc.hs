{-# LANGUAGE LambdaCase #-}

-- | Random concurrency utils.
module Ki.Internal.Conc
  ( blockUntilTVar,
    registerBlock,
  )
where

import Control.Monad (unless)
import Ki.Sig (IO, STM, TVar, readTVar, registerDelay, retry)
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
