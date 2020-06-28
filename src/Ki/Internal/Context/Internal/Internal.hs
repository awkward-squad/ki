{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Internal.Context.Internal.Internal
  ( -- * Context
    Context,
    new,
    derive,
    cancel,
    cancelled,
    matchCancelled,
    pattern Cancelled,
  )
where

import Control.Exception (fromException)
import qualified Data.IntMap.Strict as IntMap
import Ki.Internal.Concurrency
import Ki.Internal.Prelude

newtype Context
  = Context (TVar E)

data E -- strict either (StrictData)
  = L CancelToken
  | R Context_

data Context_ = Context_
  { -- | The next id to assign to a child context. The child needs a unique identifier so it can delete itself from its
    -- parent's children map if it's cancelled independently. Wrap-around seems ok; that's a *lot* of children for one
    -- parent to have.
    nextId :: Int,
    children :: IntMap Context,
    -- | When I'm cancelled, this action removes myself from my parent's context. This isn't simply a pointer to the
    -- parent (i.e. Context_) for three reasons:
    --
    --   * "Root" contexts don't have a parent, so it'd have to be a Maybe (one more pointer indirection)
    --   * We don't really need a reference to the parent, because we only want to be able to remove ourselves from its
    --     children map, so just storing the STM action that does exactly seems a bit safer, even if conceptually it's
    --     a bit indirect.
    --   * If we stored a reference to the parent, we'd also have to store our own id, rather than just currying it into
    --     this action.
    onCancel :: STM ()
  }

newtype Cancelled_
  = Cancelled_ CancelToken
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

newtype CancelToken
  = CancelToken Int
  deriving stock (Eq, Show)

-- | A 'Cancelled' exception is thrown when a __thread__ voluntarily capitulates after observing its __context__ is
-- /cancelled/.
pattern Cancelled :: Cancelled_
pattern Cancelled <- Cancelled_ _

{-# COMPLETE Cancelled #-}

new :: IO Context
new =
  coerce @(IO (TVar E)) (newTVarIO (R (Context_ {nextId, children, onCancel})))
  where
    nextId = 0 :: Int
    children = IntMap.empty :: IntMap Context
    onCancel = pure () :: STM ()

newWith :: STM () -> STM Context
newWith onCancel =
  coerce @(STM (TVar E)) (newTVar (R (Context_ {nextId, children, onCancel})))
  where
    nextId = 0 :: Int
    children = IntMap.empty :: IntMap Context

derive :: Context -> STM Context
derive context@(Context parentVar) =
  readTVar parentVar >>= \case
    R ctx@Context_ {nextId, children} -> do
      child <- newWith (deleteChild context nextId)
      writeTVar parentVar $! R ctx {nextId = nextId + 1, children = IntMap.insert nextId child children}
      pure child
    L _ -> pure context

deleteChild :: Context -> Int -> STM ()
deleteChild (Context parentVar) childId =
  readTVar parentVar >>= \case
    R ctx@Context_ {children} -> writeTVar parentVar $! R ctx {children = IntMap.delete childId children}
    L _ -> pure ()

cancel :: Context -> IO ()
cancel context = do
  token <- uniqueInt
  atomically (cancelSTM context (CancelToken token))

cancelSTM :: Context -> CancelToken -> STM ()
cancelSTM (Context contextVar) token =
  readTVar contextVar >>= \case
    R Context_ {children, onCancel} -> do
      writeTVar contextVar (L token)
      for_ (IntMap.elems children) (cancelSTM_ token)
      onCancel
    L _ -> pure ()

cancelSTM_ :: CancelToken -> Context -> STM ()
cancelSTM_ token (Context contextVar) =
  readTVar contextVar >>= \case
    R Context_ {children} -> do
      writeTVar contextVar (L token)
      for_ (IntMap.elems children) (cancelSTM_ token)
    L _ -> pure ()

cancelled :: Context -> STM (IO a)
cancelled context = do
  token <- cancelled_ context
  pure (throwIO (Cancelled_ token))

cancelled_ :: Context -> STM CancelToken
cancelled_ (Context contextVar) =
  readTVar contextVar >>= \case
    R _ -> retry
    L token -> pure token

matchCancelled :: Context -> SomeException -> STM Bool
matchCancelled context exception =
  case fromException exception of
    Just (Cancelled_ token) -> ((== token) <$> cancelled_ context) <|> pure False
    Nothing -> pure False
