{-# LANGUAGE StrictData #-}

module Ki.Indef.Context
  ( -- * Context
    Context,
    background,
    derive,

    -- * Cancellation
    cancel,
    Internal.CancelToken (..),
    cancelled,
    cancelledSTM,
    Internal.Cancelled (..),
  )
where

import Ki.Internal.Concurrency
import qualified Ki.Internal.Context.Internal as Internal
import Ki.Internal.Prelude

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ forked within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
data Context
  = Background
  | Context (TVar Internal.Context)
  deriving stock (Generic)

background :: Context
background =
  Background

-- | Derive a child context from a parent context.
--
--   * If the parent is already cancelled, so is the child.
--   * If the parent isn't already canceled, the child registers itself with the
--     parent such that:
--       * When the parent is cancelled, so is the child
--       * When the child is cancelled, it removes the parent's reference to it
derive :: Context -> STM Context
derive = \case
  Background -> Context <$> Internal.empty
  Context contextVar -> Context <$> Internal.derive contextVar

cancelled :: Context -> IO (Maybe Internal.CancelToken)
cancelled = \case
  Background -> pure Nothing
  Context contextVar -> atomically (Internal.cancelled contextVar)

cancelledSTM :: Context -> STM (Maybe Internal.CancelToken)
cancelledSTM = \case
  Background -> pure Nothing
  Context contextVar -> Internal.cancelled contextVar

cancel :: Context -> Internal.CancelToken -> STM ()
cancel context token =
  case context of
    Background -> pure ()
    Context contextVar -> Internal.cancel contextVar token
