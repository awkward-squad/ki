module Ki.Experimental.Implicit.Puller
  ( puller,
  )
where

import qualified Ki.Experimental.Puller
import qualified Ki.Implicit.Context
import Ki.Prelude
import qualified Ki.Scope

puller :: Ki.Scope.Scope -> (Ki.Implicit.Context.Context => IO a -> IO ()) -> IO (a -> STM ())
puller scope action =
  Ki.Experimental.Puller.puller scope (let ?context = Ki.Scope.context scope in action)
