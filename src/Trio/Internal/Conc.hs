{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Trio.Internal.Conc
  ( try
  , untilUninterrupted
  , pattern NotThreadKilled
  ) where

import Control.Exception (AsyncException(ThreadKilled), Exception(fromException), SomeException)
import Control.Monad.Conc.Class
import Data.Function


try :: ( Exception e, MonadConc m ) => m a -> m ( Either e a )
try action =
  catch ( Right <$> action) ( pure . Left )

-- | Execute an IO action until it successfully completes, ignoring all
-- synchronous and asynchronous exceptions.
untilUninterrupted :: MonadConc m => m () -> m ()
untilUninterrupted action =
  fix \again ->
    catch @_ @SomeException action \_ -> again

pattern NotThreadKilled :: SomeException -> SomeException
pattern NotThreadKilled ex <- ( asNotThreadKilled -> Just ex )

asNotThreadKilled :: SomeException -> Maybe SomeException
asNotThreadKilled ex =
  case fromException ex of
    Just ThreadKilled -> Nothing
    _ -> Just ex
