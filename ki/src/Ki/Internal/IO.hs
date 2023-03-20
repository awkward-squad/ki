{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Miscellaneous IO utilities
module Ki.Internal.IO
  ( -- * Unexceptional IO
    UnexceptionalIO (..),
    IOResult (..),
    unexceptionalTry,
    unexceptionalTryEither,

    -- * Exception utils
    isAsyncException,
    interruptiblyMasked,
    uninterruptiblyMasked,
    tryEitherSTM,

    -- * Fork utils
    forkIO,
    forkOn,
  )
where

import Control.Exception
import Control.Monad (join)
import Data.Coerce (coerce)
import GHC.Base (maskAsyncExceptions#, maskUninterruptible#)
import GHC.Conc (STM, ThreadId (ThreadId), catchSTM)
import GHC.Exts (Int (I#), fork#, forkOn#)
import GHC.IO (IO (IO))
import Prelude

-- A little promise that this IO action cannot throw an exception.
--
-- Yeah it's verbose, and maybe not that necessary, but the code that bothers to use it really does require
-- un-exceptiony IO actions for correctness, so here we are.
newtype UnexceptionalIO a = UnexceptionalIO
  {runUnexceptionalIO :: IO a}
  deriving newtype (Applicative, Functor, Monad)

data IOResult a
  = Failure !SomeException -- sync or async exception
  | Success a

unexceptionalTry :: forall a. IO a -> UnexceptionalIO (IOResult a)
unexceptionalTry action =
  UnexceptionalIO do
    (Success <$> action) `catch` \exception ->
      pure (Failure exception)

-- Like try, but with continuations. Also, catches all exceptions, because that's the only flavor we need.
unexceptionalTryEither ::
  forall a b.
  (SomeException -> UnexceptionalIO b) ->
  (a -> UnexceptionalIO b) ->
  IO a ->
  UnexceptionalIO b
unexceptionalTryEither onFailure onSuccess action =
  UnexceptionalIO do
    join do
      catch
        (coerce @_ @(a -> IO b) onSuccess <$> action)
        (pure . coerce @_ @(SomeException -> IO b) onFailure)

isAsyncException :: SomeException -> Bool
isAsyncException exception =
  case fromException @SomeAsyncException exception of
    Nothing -> False
    Just _ -> True

-- | Call an action with asynchronous exceptions interruptibly masked.
interruptiblyMasked :: IO a -> IO a
interruptiblyMasked (IO io) =
  IO (maskAsyncExceptions# io)

-- | Call an action with asynchronous exceptions uninterruptibly masked.
uninterruptiblyMasked :: IO a -> IO a
uninterruptiblyMasked (IO io) =
  IO (maskUninterruptible# io)

-- Like try, but with continuations
tryEitherSTM :: Exception e => (e -> STM b) -> (a -> STM b) -> STM a -> STM b
tryEitherSTM onFailure onSuccess action =
  join (catchSTM (onSuccess <$> action) (pure . onFailure))

-- Control.Concurrent.forkIO without the exception handler
forkIO :: IO () -> IO ThreadId
forkIO (IO action) =
  IO \s0 ->
    case fork# action s0 of
      (# s1, tid #) -> (# s1, ThreadId tid #)

-- Control.Concurrent.forkOn without the exception handler
forkOn :: Int -> IO () -> IO ThreadId
forkOn (I# cap) (IO action) =
  IO \s0 ->
    case forkOn# cap action s0 of
      (# s1, tid #) -> (# s1, ThreadId tid #)
