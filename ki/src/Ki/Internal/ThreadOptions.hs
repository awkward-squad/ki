module Ki.Internal.ThreadOptions
  ( ThreadOptions (..),
    defaultThreadOptions,
  )
where

import Control.Exception (MaskingState (..))
import Ki.Internal.ByteCount (ByteCount)
import Ki.Internal.ThreadAffinity (ThreadAffinity (..))

-- |
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
--     The label of a thread, visible in the [event log](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/runtime_control.html#rts-eventlog) (@+RTS -l@).
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
--   { 'Ki.affinity' = 'Ki.Unbound'
--   , 'Ki.allocationLimit' = Nothing
--   , 'Ki.label' = ""
--   , 'Ki.maskingState' = 'Unmasked'
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
