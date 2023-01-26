-- | See "Ki" for documentation.
--
module Ki.Typed (

  -- * Demos
  -- $Demos
  concurrently,
  race,

  -- * Unlifting
  -- $Unlifting

  -- * Core API
  Scoped,
  scoped,
  mapScoped,

  Thread,
  fork,
  forkTry,
  await,
  awaitAll,

  -- * Extended API
  fork_,
  forkWith,
  forkWith_,
  forkTryWith,

  -- ** Thread options
  ThreadOptions,
  Ki.defaultThreadOptions,
  ThreadAffinity,

  -- ** Byte count
  ByteCount,
  Ki.kilobytes,
  Ki.megabytes,

) where

import Data.Void (Void)
import GHC.Conc (STM, atomically)
import Control.Applicative (Alternative(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO(..))

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT, ask)

import Ki.Internal.Prelude
import Ki (ThreadOptions, ThreadAffinity, ByteCount)
import qualified Ki


-- $Demos
--
-- The "Quick start examples" can be written much the same as with "Ki", but
-- require some lifting.

concurrently :: Scoped s IO a -> Scoped s IO b -> Scoped s IO (a, b)
concurrently action1 action2 = do
  thread1 <- fork action1
  result2 <- action2
  result1 <- mapScoped atomically (await thread1)
  pure (result1, result2)

race :: Scoped s IO a -> Scoped s IO a -> Scoped s IO a
race action1 action2 = do
  resultVar <- liftIO newEmptyMVar
  _ <- fork (action1 >>= liftIO . tryPutMVar resultVar)
  _ <- fork (action2 >>= liftIO . tryPutMVar resultVar)
  liftIO (takeMVar resultVar)


-- $Unlifting
--
-- Unfortunately, 'Scoped' cannot safely support unlifting; it cannot have
-- @MonadUnliftIO@. If we were to expose
--
-- > withRunScopedInF
-- >   :: ((forall a. Scoped s f a -> f a) -> f b) -> Scoped s f b
--
-- then it would be possible to write, e.g.,
--
-- > sneakOutFork_ :: IO (IO Void -> IO ())
-- > sneakOutFork_ = scoped $ withRunScopedInF \run ->
-- >   pure (run . fork_ . lift)
--
-- Here, @run@ is efectively an @unsafeRunScoped@, and it captures a 'Ki.Scope'.
-- As such, producing a function that uses it allows one to sneak a 'Ki.Scope'
-- out of 'Scoped', breaking the safety of the interface.


-- Core API

newtype Scoped s f a = Scoped{ runScoped :: ReaderT Ki.Scope f a }
  deriving newtype
    ( Functor, Applicative, Alternative
    , Monad, MonadIO, MonadFix, MonadTrans
    )
type role Scoped nominal representational nominal

scoped :: (forall s. Scoped s IO a) -> IO a
scoped action = Ki.scoped (coerce action)

mapScoped :: (f a -> g b) -> Scoped s f a -> Scoped s g b
mapScoped f (Scoped rsfa) = Scoped (mapReaderT f rsfa)

newtype Thread s a = Thread (Ki.Thread a)
type role Thread nominal representational

fork :: Scoped s IO a -> Scoped s IO (Thread s a)
fork s = Thread <$> withScope Ki.fork s

forkTry :: Exception e => Scoped s IO a -> Scoped s IO (Thread s (Either e a))
forkTry s = Thread <$> withScope Ki.forkTry s

await :: Thread s a -> Scoped s STM a
await (Thread t) = lift (Ki.await t)

awaitAll :: Scoped s STM ()
awaitAll = Scoped ask >>= lift . Ki.awaitAll


-- Extended API

fork_ :: Scoped s IO Void -> Scoped s IO ()
fork_ = withScope Ki.fork_

forkWith :: ThreadOptions -> Scoped s IO a -> Scoped s IO (Thread s a)
forkWith opts s = Thread <$> withScope (flip Ki.forkWith opts) s

forkWith_ :: ThreadOptions -> Scoped s IO Void -> Scoped s IO ()
forkWith_ = withScope . flip Ki.forkWith_

forkTryWith
  :: Exception e
  => ThreadOptions -> Scoped s IO a -> Scoped s IO (Thread s (Either e a))
forkTryWith opts s = Thread <$> withScope (flip Ki.forkTryWith opts) s


-- Internal Utils

withScope
  :: (Ki.Scope -> IO a -> IO b)
  -> Scoped s IO a -> Scoped s IO b
withScope k s = do
  scope <- Scoped ask
  withRunScopedInF \run ->
    k scope (run s)

withRunScopedInF :: ((forall a. Scoped s f a -> f a) -> f b) -> Scoped s f b
withRunScopedInF k = Scoped $ ReaderT \scope -> k \action ->
  runReaderT (runScoped action) scope

