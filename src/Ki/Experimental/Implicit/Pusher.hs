module Ki.Experimental.Implicit.Pusher
  ( pusher,
    pusher2,
  )
where

import qualified Ki.Experimental.Pusher as Ki.Pusher
import Ki.Implicit.Context (Context)
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope

pusher :: Scope -> (Context => (a -> IO ()) -> IO ()) -> IO (STM (Maybe a))
pusher scope action =
  Ki.Pusher.pusher scope (let ?context = Ki.Scope.context scope in action)

pusher2 :: Scope -> (Context => (a -> IO ()) -> IO ()) -> IO (STM (Maybe a))
pusher2 scope action =
  Ki.Pusher.pusher2 scope (let ?context = Ki.Scope.context scope in action)
