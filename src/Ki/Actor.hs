module Ki.Actor
  ( Actor,
    actor,
    send,
  )
where

import Ki.Concurrency
import qualified Ki.Fork
import qualified Ki.Mailbox
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope

newtype Actor a
  = Actor (a -> STM ())

actor :: Scope -> s -> (s -> a -> IO s) -> IO (Actor a)
actor scope s0 action = do
  mailbox <- Ki.Mailbox.new

  Ki.Fork.fork scope do
    (`fix` s0) \loop s ->
      Ki.Scope.unlessCancelledSTM scope do
        x <- Ki.Mailbox.receive mailbox
        pure (action s x >>= loop)

  pure (Actor (Ki.Mailbox.send mailbox))

send :: Actor a -> a -> STM ()
send (Actor f) =
  f
