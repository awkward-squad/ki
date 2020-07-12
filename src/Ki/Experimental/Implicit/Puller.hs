module Ki.Experimental.Implicit.Puller
  ( puller,
  )
where

import qualified Ki.Experimental.Puller as Ki.Puller
import Ki.Implicit.Context (Context)
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope

puller :: Scope -> (Context => IO a -> IO ()) -> IO (a -> STM ())
puller scope action =
  Ki.Puller.puller scope (let ?context = Ki.Scope.context scope in action)
