module Ki.Internal.ThreadAffinity
  ( ThreadAffinity (..),
  )
where

-- | What, if anything, a thread is bound to.
data ThreadAffinity
  = -- | Unbound.
    Unbound
  | -- | Bound to a capability.
    Capability Int
  | -- | Bound to an OS thread.
    OsThread
  deriving stock (Eq, Show)
