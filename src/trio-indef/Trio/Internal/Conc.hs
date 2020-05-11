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
import Data.Function
import Trio.Sig
import Prelude hiding (IO)

blockUntilTVar :: TVar a -> (a -> Bool) -> STM ()
blockUntilTVar var f = do
  value <- readTVar var
  unless (f value) retry

-- | Execute an IO action until it successfully completes, ignoring all
-- synchronous and asynchronous exceptions.
retryingUntilSuccess :: IO a -> IO a
retryingUntilSuccess action =
  fix \again ->
    catch @SomeException action \_ -> again

pattern NotThreadKilled :: SomeException -> SomeException
pattern NotThreadKilled ex <- (asNotThreadKilled -> Just ex)

asNotThreadKilled :: SomeException -> Maybe SomeException
asNotThreadKilled ex =
  case fromException ex of
    Just ThreadKilled -> Nothing
    _ -> Just ex
