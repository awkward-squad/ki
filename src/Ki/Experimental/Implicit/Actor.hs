module Ki.Experimental.Implicit.Actor
  ( Actor,
    actor,
    Ki.Actor.send,
  )
where

import Ki.Experimental.Actor (Actor)
import qualified Ki.Experimental.Actor as Ki.Actor
import Ki.Concurrency
import Ki.Implicit.Context (Context)
import qualified Ki.Scope
import Ki.Scope (Scope)

actor :: Scope -> s -> (Context => s -> a -> IO s) -> IO (Actor a)
actor scope s0 action =
  Ki.Actor.actor scope s0 (let ?context = Ki.Scope.context scope in action)
