module Ki.Experimental.Implicit.Pusher
  ( pusher,
  )
where

import qualified Ki.Experimental.Pusher
import qualified Ki.Implicit.Context
import Ki.Prelude
import qualified Ki.Scope

pusher :: Ki.Scope.Scope -> (Ki.Implicit.Context.Context => (a -> IO ()) -> IO ()) -> IO (STM (Maybe a))
pusher scope action =
  Ki.Experimental.Pusher.pusher scope (let ?context = Ki.Scope.context scope in action)
