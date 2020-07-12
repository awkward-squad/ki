module Ki.Experimental.Implicit.Actor
  ( actor,
  )
where

import Ki.Concurrency
import qualified Ki.Experimental.Actor as Ki.Actor
import Ki.Implicit.Context (Context)
import qualified Ki.Scope
import Ki.Scope (Scope)

actor :: Scope -> s -> (Context => s -> a -> IO s) -> IO (a -> STM ())
actor scope s0 action =
  Ki.Actor.actor scope s0 (let ?context = Ki.Scope.context scope in action)
