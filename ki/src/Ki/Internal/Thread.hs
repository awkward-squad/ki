module Ki.Internal.Thread
  ( Thread,
    makeThread,
    await,
    ThreadAffinity (..),
    ThreadOptions (..),
    defaultThreadOptions,
    ThreadFailed (..),
    unwrapThreadFailed,
  )
where

import Control.Exception
  ( BlockedIndefinitelyOnSTM (..),
    Exception (fromException, toException),
    MaskingState (..),
    asyncExceptionFromException,
    asyncExceptionToException,
  )
import GHC.Conc
  ( STM,
  )
import Ki.Internal.ByteCount
import Ki.Internal.Prelude

-- | A thread, parameterized by the type of value it computes.
--
-- ==== __👉 Details__
--
-- * Each thread is associated with a single /expected/ exception type (which, due to the extensible exception
-- hierarchy, is actually an arbitrary tree of exception types).
--
-- * If an /unexpected/ exception is raised in a thread, the exception is propagated to the thread's parent.
--
-- * By default (see 'fork'), the expected exception type is "no exception", which means any exception raised in a child
-- is propagated to its parent. Use 'forktry' to expect a different exception type.
--
-- * Asynchronous exceptions (i.e. exception types that are instances of 'Control.Exception.SomeAsyncException') are
-- always considered unexpected, and thus always propagated, except for the internal asynchronous exception this library
-- uses to terminate the children that were created within a scope that is closing.
--
-- * An individual thread cannot be terminated explicitly ala 'Control.Concurrent.killThread'. However, all threads
-- created within a scope are terminated when the scope closes.
data Thread a
  = Thread {-# UNPACK #-} !ThreadId !(STM a)
  deriving stock (Functor)

instance Eq (Thread a) where
  Thread ix _ == Thread iy _ =
    ix == iy

instance Ord (Thread a) where
  compare (Thread ix _) (Thread iy _) =
    compare ix iy

makeThread :: ThreadId -> STM a -> Thread a
makeThread threadId action0 =
  Thread
    threadId
    -- If *they* are deadlocked, we will *both* will be delivered a wakeup from the RTS. We want to shrug this exception
    -- off, because afterwards they'll have put to the result var. But don't shield indefinitely, once will cover this
    -- use case and prevent any accidental infinite loops.
    (tryEitherSTM (\BlockedIndefinitelyOnSTM -> action0) pure action0)

-- | What, if anything, a thread is bound to.
data ThreadAffinity
  = -- | Unbound.
    Unbound
  | -- | Bound to a capability.
    Capability Int
  | -- | Bound to an OS thread.
    OsThread
  deriving stock (Eq, Show)

-- | Thread options that can be provided at the time a thread is created.
--
-- [@affinity@]:
--
--     The affinity of a thread. A thread can be unbound, bound to a specific capability, or bound to a specific OS
--     thread.
--
--     Default: 'Unbound'
--
-- [@allocationLimit@]:
--
--     The maximum number of bytes a thread may allocate before it is delivered an
--     'Control.Exception.AllocationLimitExceeded' exception. If caught, the thread is allowed to allocate an additional
--     100kb (tunable with @+RTS -xq@) to perform any necessary cleanup actions; if exceeded, the thread is delivered
--     another.
--
--     Default: @Nothing@ (no limit)
--
-- [@label@]:
--
--     The label of a thread, used in the event log.
--
--     Default: @""@ (no label)
--
-- [@maskingState@]:
--
--     The masking state a thread is created in. To unmask, use 'GHC.IO.unsafeUnmask'.
--
--     Default: @Unmasked@
data ThreadOptions = ThreadOptions
  { affinity :: ThreadAffinity,
    allocationLimit :: Maybe ByteCount,
    label :: String,
    maskingState :: MaskingState
  }
  deriving stock (Eq, Show)

-- | Default thread options.
--
-- @
-- 'Ki.ThreadOptions'
--   { 'Ki.affinity' = Nothing
--   , 'Ki.allocationLimit' = Nothing
--   , 'Ki.label' = ""
--   , 'Ki.maskingState' = Unmasked
--   }
-- @
defaultThreadOptions :: ThreadOptions
defaultThreadOptions =
  ThreadOptions
    { affinity = Unbound,
      allocationLimit = Nothing,
      label = "",
      maskingState = Unmasked
    }

-- Internal exception type thrown by a child thread to its parent, if it fails unexpectedly.
newtype ThreadFailed
  = ThreadFailed SomeException
  deriving stock (Show)

instance Exception ThreadFailed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

unwrapThreadFailed :: SomeException -> SomeException
unwrapThreadFailed e0 =
  case fromException e0 of
    Just (ThreadFailed e1) -> e1
    Nothing -> e0

-- | Wait for a thread to terminate, and return its value.
await ::
  -- |
  Thread a ->
  STM a
await (Thread _threadId doAwait) =
  -- If *they* are deadlocked, we will *both* will be delivered a wakeup from the RTS. We want to shrug this exception
  -- off, because afterwards they'll have put to the result var. But don't shield indefinitely, once will cover this use
  -- case and prevent any accidental infinite loops.
  tryEitherSTM (\BlockedIndefinitelyOnSTM -> doAwait) pure doAwait
