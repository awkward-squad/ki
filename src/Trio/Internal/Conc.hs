{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Random concurrency utils.
module Trio.Internal.Conc
  ( blockUntilTVar,
    retryingUntilSuccess,
    try,
    pattern NotThreadKilled,
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (fromException), SomeException)
import Control.Monad
import Control.Monad.Conc.Class
import Control.Monad.STM.Class
import Data.Function

blockUntilTVar :: MonadSTM stm => TVar stm a -> (a -> Bool) -> stm ()
blockUntilTVar var f = do
  value <- readTVar var
  unless (f value) retry

-- | Execute an IO action until it successfully completes, ignoring all
-- synchronous and asynchronous exceptions.
retryingUntilSuccess :: MonadConc m => m a -> m a
retryingUntilSuccess action =
  fix \again ->
    catch @_ @SomeException action \_ -> again

try :: (Exception e, MonadConc m) => m a -> m (Either e a)
try action =
  catch (Right <$> action) (pure . Left)

pattern NotThreadKilled :: SomeException -> SomeException
pattern NotThreadKilled ex <- (asNotThreadKilled -> Just ex)

asNotThreadKilled :: SomeException -> Maybe SomeException
asNotThreadKilled ex =
  case fromException ex of
    Just ThreadKilled -> Nothing
    _ -> Just ex
