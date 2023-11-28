module Ki.Internal.ThreadAffinity
  ( ThreadAffinity (..),
    forkWithAffinity,
  )
where

import Control.Concurrent (ThreadId, forkOS)
import Ki.Internal.IO (forkIO, forkOn)

-- | What, if anything, a thread is bound to.
data ThreadAffinity
  = -- | Unbound.
    Unbound
  | -- | Bound to a capability.
    Capability Int
  | -- | Bound to an OS thread.
    OsThread
  deriving stock (Eq, Show)

-- forkIO/forkOn/forkOS, switching on affinity
forkWithAffinity :: ThreadAffinity -> IO () -> IO ThreadId
forkWithAffinity = \case
  Unbound -> forkIO
  Capability n -> forkOn n
  OsThread -> forkOS
