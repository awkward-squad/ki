-- | STM minus retry. These STM actions are guaranteed not to block, and thus guaranteed not to be interrupted by an
-- async exception.
module Ki.Internal.NonblockingSTM
  ( NonblockingSTM,
    nonblockingAtomically,
    nonblockingThrowSTM,

    -- * TVar
    nonblockingReadTVar,
    nonblockingWriteTVar',
  )
where

import Control.Exception (Exception)
import Data.Coerce (coerce)
import GHC.Conc (STM, TVar, atomically, readTVar, throwSTM, writeTVar)

newtype NonblockingSTM a
  = NonblockingSTM (STM a)
  deriving newtype (Applicative, Functor, Monad)

nonblockingAtomically :: forall a. NonblockingSTM a -> IO a
nonblockingAtomically =
  coerce @(STM a -> IO a) atomically

nonblockingThrowSTM :: forall e x. (Exception e) => e -> NonblockingSTM x
nonblockingThrowSTM =
  coerce @(e -> STM x) throwSTM

nonblockingReadTVar :: forall a. TVar a -> NonblockingSTM a
nonblockingReadTVar =
  coerce @(TVar a -> STM a) readTVar

nonblockingWriteTVar' :: forall a. TVar a -> a -> NonblockingSTM ()
nonblockingWriteTVar' var !x =
  NonblockingSTM (writeTVar var x)
