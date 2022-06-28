{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Ki.Internal.Prelude
  ( forkIO,
    forkOn,
    interruptiblyMasked,
    uninterruptiblyMasked,
    module X,
  )
where

import Control.Applicative as X (optional, (<|>))
import Control.Concurrent hiding (forkIO, forkOn)
import Control.Concurrent as X (ThreadId, myThreadId, threadDelay, throwTo)
import Control.Concurrent.MVar as X
import Control.Exception
import Control.Exception as X (Exception, SomeException, mask_, throwIO, try, uninterruptibleMask, uninterruptibleMask_)
import Control.Monad as X (join, when)
import Data.Coerce as X (coerce)
import Data.Data as X (Data)
import Data.Foldable as X (for_, traverse_)
import Data.Function as X (fix)
import Data.Functor as X (void, ($>), (<&>))
import Data.Int as X
import Data.IntMap.Strict as X (IntMap)
import Data.Map.Strict as X (Map)
import Data.Maybe as X (fromMaybe)
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.Word as X (Word32)
import GHC.Base (maskAsyncExceptions#, maskUninterruptible#)
import GHC.Conc (ThreadId (ThreadId))
import GHC.Exts (Int (I#), fork#, forkOn#)
import GHC.Generics as X (Generic)
import GHC.IO (IO (IO))
import Numeric.Natural as X (Natural)
import Prelude as X

-- | Call an action with asynchronous exceptions interruptibly masked.
interruptiblyMasked :: IO a -> IO a
interruptiblyMasked (IO io) =
  IO (maskAsyncExceptions# io)

-- | Call an action with asynchronous exceptions uninterruptibly masked.
uninterruptiblyMasked :: IO a -> IO a
uninterruptiblyMasked (IO io) =
  IO (maskUninterruptible# io)

-- Control.Concurrent.forkIO without the dumb exception handler
forkIO :: IO () -> IO ThreadId
forkIO action =
  IO \s0 ->
    case fork# action s0 of
      (# s1, tid #) -> (# s1, ThreadId tid #)

-- Control.Concurrent.forkOn without the dumb exception handler
forkOn :: Int -> IO () -> IO ThreadId
forkOn (I# cap) action =
  IO \s0 ->
    case forkOn# cap action s0 of
      (# s1, tid #) -> (# s1, ThreadId tid #)
