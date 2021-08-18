-- The Unique type exported by this module is repurposed from
-- https://github.com/ekmett/unique/tree/e3499e1633a2e974aaeba132993fd34b4f113b2b. License below.
--
-- Copyright 2015 Edward Kmett
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Ki.Internal.Prelude
  ( Unique,
    atomicallyIO,
    debug,
    forkIO,
    newUnique,
    onLeft,
    putTMVarIO,
    registerDelay,
    whenJust,
    whenLeft,
    whenM,
    module X,
  )
where

import Control.Applicative as X (optional, (<|>))
import Control.Concurrent hiding (forkIO)
import Control.Concurrent as X (ThreadId, myThreadId, threadDelay, throwTo)
import Control.Concurrent.STM as X hiding (registerDelay)
import Control.Exception
import Control.Exception as X (Exception, SomeException, throwIO, try, uninterruptibleMask)
import Control.Monad (unless)
import Control.Monad as X (join, unless)
import Control.Monad.IO.Class as X (MonadIO (..))
import Data.Coerce as X (coerce)
import Data.Foldable as X (for_)
import Data.Function as X (fix)
import Data.Functor as X (void, ($>), (<&>))
import Data.IntMap.Strict as X (IntMap)
import Data.Map.Strict as X (Map)
import Data.Maybe as X (fromMaybe)
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.Word as X (Word32)
import GHC.Conc (ThreadId (ThreadId))
#if defined(mingw32_HOST_OS)
import GHC.Conc.Windows
#else
import GHC.Event
#endif

import GHC.Exts
  ( Int (I#),
    MutableByteArray#,
    RealWorld,
    addr2Int#,
    fork#,
    isTrue#,
    newByteArray#,
    sameMutableByteArray#,
    unsafeCoerce#,
  )
import GHC.Generics as X (Generic)
import GHC.IO (IO (IO))
import GHC.IO as X (unsafeUnmask)
import System.IO.Unsafe (unsafePerformIO)
import Prelude as X

-- FIXME UnliftedNewtypes when it's old enough (introduced 8.10)
data Unique
  = Unique (MutableByteArray# RealWorld)

instance Eq Unique where
  Unique x == Unique y =
    isTrue# (sameMutableByteArray# x y)

instance Show Unique where
  show :: Unique -> String
  show (Unique x) =
    show (I# (addr2Int# (unsafeCoerce# x)))

atomicallyIO :: STM (IO a) -> IO a
atomicallyIO =
  join . atomically

debug :: Monad m => String -> m ()
debug message =
  unsafePerformIO output `seq` pure ()
  where
    output :: IO ()
    output = do
      threadId <- myThreadId
      withMVar lock \_ -> putStrLn ("[" ++ show threadId ++ "] " ++ message)

lock :: MVar ()
lock =
  unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}

-- Control.Concurrent.forkIO without the dumb exception handler
forkIO :: IO () -> IO ThreadId
forkIO action =
  IO \s ->
    case fork# action s of
      (# s1, tid #) -> (# s1, ThreadId tid #)

onLeft :: (a -> IO b) -> Either a b -> IO b
onLeft f =
  either f pure

newUnique :: IO Unique
newUnique =
  IO \s0 ->
    case newByteArray# 0# s0 of
      (# s1, x #) -> (# s1, Unique x #)

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO var x =
  atomically (putTMVar var x)

#if defined(mingw32_HOST_OS)
registerDelay :: Int -> IO (STM (), IO ())
registerDelay micros = do
  var <- GHC.Conc.Windows.registerDelay micros
  pure (readTVar var >>= \b -> unless b retry, pure ()) -- no unregister on Windows =P
#else
registerDelay :: Int -> IO (STM (), IO ())
registerDelay micros = do
  var <- newTVarIO False
  manager <- getSystemTimerManager
  key <- registerTimeout manager micros (atomically (writeTVar var True))
  pure (readTVar var >>= \b -> unless b retry, unregisterTimeout manager key)
#endif

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust x f =
  maybe (pure ()) f x

whenLeft :: Either a b -> (a -> IO b) -> IO b
whenLeft x f =
  either f pure x

whenM :: IO Bool -> IO () -> IO ()
whenM x y =
  x >>= \case
    False -> pure ()
    True -> y
