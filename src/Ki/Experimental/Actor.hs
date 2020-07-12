module Ki.Experimental.Actor
  ( actor,
  )
where

import qualified Ki.Experimental.Mailbox as Ki.Mailbox
import qualified Ki.Fork
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope

actor :: Scope -> s -> (s -> a -> IO s) -> IO (a -> STM ())
actor scope s0 action = do
  mailbox <- Ki.Mailbox.new

  Ki.Fork.fork scope do
    (`fix` s0) \loop s ->
      Ki.Scope.unlessCancelledSTM scope do
        x <- Ki.Mailbox.receive mailbox
        pure (action s x >>= loop)

  pure (Ki.Mailbox.send mailbox)
