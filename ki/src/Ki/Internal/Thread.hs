module Ki.Internal.Thread
  ( Thread,
    makeThread,
    await,
  )
where

import Control.Concurrent (ThreadId)
import Control.Exception (BlockedIndefinitelyOnSTM (..))
import GHC.Conc (STM)
import Ki.Internal.IO (tryEitherSTM)

-- | A thread.
--
-- ==== __👉 Details__
--
-- * A thread's lifetime is delimited by the scope in which it was created.
--
-- * The thread that creates a scope is considered the parent of all threads created within it.
--
-- * If an exception is raised in a child thread, the child either propagates the exception to its parent (see
--   'Ki.fork'), or returns the exception as a value (see 'Ki.forkTry').
--
-- * All threads created within a scope are terminated when the scope closes.
data Thread a = Thread
  { threadId :: {-# UNPACK #-} !ThreadId,
    await_ :: !(STM a)
  }
  deriving stock (Functor)

instance Eq (Thread a) where
  Thread ix _ == Thread iy _ =
    ix == iy

instance Ord (Thread a) where
  compare (Thread ix _) (Thread iy _) =
    compare ix iy

makeThread :: ThreadId -> STM a -> Thread a
makeThread threadId action =
  Thread
    { threadId,
      -- If *they* are deadlocked, we will *both* will be delivered a wakeup from the RTS. We want to shrug this
      -- exception off, because afterwards they'll have put to the result var. But don't shield indefinitely, once will
      -- cover this use case and prevent any accidental infinite loops.
      await_ = tryEitherSTM (\BlockedIndefinitelyOnSTM -> action) pure action
    }

-- | Wait for a thread to terminate.
await :: Thread a -> STM a
await =
  await_
