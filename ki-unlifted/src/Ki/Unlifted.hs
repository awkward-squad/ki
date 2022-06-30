-- | The `ki` API, generalized to use 'MonadUnliftIO'.
--
-- __Note__: See "Ki" for the main module documentation. Any documentation you see here is incidental, and only a result
-- of re-exporting symbols directly from "Ki".
module Ki.Unlifted
  ( Ki.Scope,
    Ki.Thread,
    scoped,
    fork,
    forkTry,
    Ki.await,
    Ki.awaitAll,
    fork_,
    forkWith,
    forkWith_,
    forkTryWith,
    Ki.ThreadOptions (..),
    Ki.defaultThreadOptions,
    Ki.ThreadAffinity (..),
    Ki.ByteCount,
    Ki.kilobytes,
    Ki.megabytes,
  )
where

import Control.Exception (Exception)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Void (Void)
import qualified Ki
import Prelude

fork :: forall a m. MonadUnliftIO m => Ki.Scope -> m a -> m (Ki.Thread a)
fork scope action =
  withRunInIO \unlift -> Ki.fork scope (unlift action)

fork_ :: MonadUnliftIO m => Ki.Scope -> m Void -> m ()
fork_ scope action =
  withRunInIO \unlift -> Ki.fork_ scope (unlift action)

forkWith :: forall a m. MonadUnliftIO m => Ki.Scope -> Ki.ThreadOptions -> m a -> m (Ki.Thread a)
forkWith scope opts action =
  withRunInIO \unlift -> Ki.forkWith scope opts (unlift action)

forkWith_ :: MonadUnliftIO m => Ki.Scope -> Ki.ThreadOptions -> m Void -> m ()
forkWith_ scope opts action =
  withRunInIO \unlift -> Ki.forkWith_ scope opts (unlift action)

forkTry :: (Exception e, MonadUnliftIO m) => Ki.Scope -> m a -> m (Ki.Thread (Either e a))
forkTry scope action =
  withRunInIO \unlift -> Ki.forkTry scope (unlift action)

forkTryWith ::
  (Exception e, MonadUnliftIO m) =>
  Ki.Scope ->
  Ki.ThreadOptions ->
  m a ->
  m (Ki.Thread (Either e a))
forkTryWith scope opts action =
  withRunInIO \unlift -> Ki.forkTryWith scope opts (unlift action)

scoped :: forall a m. MonadUnliftIO m => (Ki.Scope -> m a) -> m a
scoped action =
  withRunInIO \unlift -> Ki.scoped \scope -> unlift (action scope)
