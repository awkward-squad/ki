-- | The `ki` API, generalized to use 'MonadIO' and 'MonadUnliftIO'.
--
-- For a variant of this API that does not use @<https://hackage.haskell.org/package/unliftio-core unliftio-core>@, see
-- @<https://hackage.haskell.org/package/ki ki>@.
module Ki.Unlifted
  ( -- * Core API
    Ki.Scope,
    scoped,
    wait,
    Ki.Thread,
    fork,
    forktry,
    await,

    -- * Extended API
    fork_,
    forkWith,
    forkWith_,
    forktryWith,
    Ki.awaitSTM,
    Ki.waitSTM,

    -- ** Thread options
    Ki.ThreadOpts (..),
    Ki.defaultThreadOpts,
    Ki.ThreadAffinity (..),

    -- ** Bytes
    Ki.Bytes,
    Ki.kilobytes,
    Ki.megabytes,
  )
where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Void (Void)
import qualified Ki
import Prelude

-- | See 'Ki.await'.
await :: forall a m. MonadIO m => Ki.Thread a -> m a
await thread =
  liftIO (Ki.await thread)

-- | See 'Ki.fork'.
fork :: forall a m. MonadUnliftIO m => Ki.Scope -> m a -> m (Ki.Thread a)
fork scope action =
  withRunInIO \unlift -> Ki.fork scope (unlift action)

-- | See 'Ki.fork_'.
fork_ :: MonadUnliftIO m => Ki.Scope -> m Void -> m ()
fork_ scope action =
  withRunInIO \unlift -> Ki.fork_ scope (unlift action)

-- | See 'Ki.forkWith'.
forkWith :: forall a m. MonadUnliftIO m => Ki.Scope -> Ki.ThreadOpts -> m a -> m (Ki.Thread a)
forkWith scope opts action =
  withRunInIO \unlift -> Ki.forkWith scope opts (unlift action)

-- | See 'Ki.forkWith_'.
forkWith_ :: MonadUnliftIO m => Ki.Scope -> Ki.ThreadOpts -> m Void -> m ()
forkWith_ scope opts action =
  withRunInIO \unlift -> Ki.forkWith_ scope opts (unlift action)

-- | See 'Ki.forktry'.
forktry :: forall e m a. (Exception e, MonadUnliftIO m) => Ki.Scope -> m a -> m (Ki.Thread (Either e a))
forktry scope action =
  withRunInIO \unlift -> Ki.forktry scope (unlift action)

-- | See 'Ki.forktryWith'.
forktryWith ::
  forall e m a.
  (Exception e, MonadUnliftIO m) =>
  Ki.Scope ->
  Ki.ThreadOpts ->
  m a ->
  m (Ki.Thread (Either e a))
forktryWith scope opts action =
  withRunInIO \unlift -> Ki.forktryWith scope opts (unlift action)

-- | See 'Ki.scoped'.
scoped :: forall a m. MonadUnliftIO m => (Ki.Scope -> m a) -> m a
scoped action =
  withRunInIO \unlift -> Ki.scoped \scope -> unlift (action scope)

-- | See 'Ki.wait'.
wait :: MonadIO m => Ki.Scope -> m ()
wait scope =
  liftIO (Ki.wait scope)
