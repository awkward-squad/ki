{-# LANGUAGE TypeApplications #-}

module Ki.Experimental.Mailbox
  ( Mailbox,
    new,
    send,
    receive,
  )
where

import Ki.Concurrency
import Ki.Prelude

type Mailbox a =
  TBQueue a

new :: IO (Mailbox a)
new =
  newTBQueueIO (2 ^ (20 :: Int))

send :: Mailbox a -> a -> STM ()
send =
  writeTBQueue

receive :: Mailbox a -> STM a
receive =
  readTBQueue
