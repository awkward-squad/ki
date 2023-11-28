module Ki.Internal.Thread
  ( Thread,
    makeThread,
    await,
    forkWithAffinity,
  )
where

import Control.Concurrent (ThreadId, forkOS)
import Control.Exception (BlockedIndefinitelyOnSTM (..))
import GHC.Conc (STM)
import Ki.Internal.IO (forkIO, forkOn, tryEitherSTM)
import Ki.Internal.ThreadAffinity (ThreadAffinity (..))

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

-- forkIO/forkOn/forkOS, switching on affinity
forkWithAffinity :: ThreadAffinity -> IO () -> IO ThreadId
forkWithAffinity = \case
  Unbound -> forkIO
  Capability n -> forkOn n
  OsThread -> Control.Concurrent.forkOS

-- | Wait for a thread to terminate.
await :: Thread a -> STM a
await =
  await_
