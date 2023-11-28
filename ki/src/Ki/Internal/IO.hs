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
    assertIO,
    assertM,
    exceptionIs,
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
import Data.Maybe (isJust)
import GHC.Base (maskAsyncExceptions#, maskUninterruptible#)
import GHC.Conc (STM, ThreadId (ThreadId), catchSTM)
import GHC.Exts (Int (I#), fork#, forkOn#)
import GHC.IO (IO (IO))
import System.IO.Unsafe (unsafePerformIO)
import Prelude

-- A little promise that this IO action cannot throw an exception (*including* async exceptions, which you normally
-- think of as being able to strike at any time).
--
-- Yeah it's verbose, and maybe not that necessary, but the code that bothers to use it really does require
-- un-exceptiony IO actions for correctness, so here we are.
newtype UnexceptionalIO a = UnexceptionalIO
  {runUnexceptionalIO :: IO a}
  deriving newtype (Applicative, Functor, Monad)

data IOResult a
  = Failure !SomeException -- sync or async exception
  | Success a

-- Try an action, catching any exception it throws.
--
-- The caller is responsible for ensuring that async exceptions are masked (at whatever masking level is appropriate),
-- as (again) `UnexceptionalIO` implies async exceptions won't be thrown either.
unexceptionalTry :: forall a. IO a -> UnexceptionalIO (IOResult a)
unexceptionalTry action =
  UnexceptionalIO do
    (Success <$> action) `catch` \exception ->
      pure (Failure exception)

-- Like try, but with continuations.
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

-- | Make an assertion in a IO that requires IO.
assertIO :: IO Bool -> IO ()
assertIO b =
  assert (unsafePerformIO b) (pure ())
{-# INLINE assertIO #-}

-- | Make an assertion in a monad.
assertM :: (Applicative m) => Bool -> m ()
assertM b =
  assert b (pure ())
{-# INLINE assertM #-}

-- | @exceptionIs \@e exception@ returns whether @exception@ is an instance of @e@.
exceptionIs :: forall e. (Exception e) => SomeException -> Bool
exceptionIs =
  isJust . fromException @e

-- | Call an action with asynchronous exceptions interruptibly masked.
interruptiblyMasked :: forall a. IO a -> IO a
interruptiblyMasked =
  coerce (maskAsyncExceptions# @a)

-- | Call an action with asynchronous exceptions uninterruptibly masked.
uninterruptiblyMasked :: forall a. IO a -> IO a
uninterruptiblyMasked =
  coerce (maskUninterruptible# @a)

-- Like try, but with continuations
tryEitherSTM :: (Exception e) => (e -> STM b) -> (a -> STM b) -> STM a -> STM b
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
