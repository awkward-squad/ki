module Ki.Internal.CancelPath
  ( CancelPath,
    emptyCancelPath,
    cancelPathPush,
    cancelPathPop,
    cancelPathCancel,
    cancelPathCancelled,
    cancelPathLeafCancelled,
  )
where

import qualified Data.Sequence as Seq
import Ki.Internal.CancelToken
import Ki.Internal.Context
import Ki.Internal.Prelude

-- | A cancel path represents a path down a tree, where each node can be cancelled with a token. When a node is
-- cancelled, all nodes underneath it are considered to be cancelled with the same token, regardless of if they were
-- already cancelled with a different token.
newtype CancelPath
  = -- Implementation notes: in order to reduce heady write loads on cancelling a node that is high up in a tree, the
    -- path structure does not always literally reflect the fact that whenever a parent node is cancelled, so are its
    -- children. Rather, to check whether a node is cancelled, we walk to it from the root, and use the first cancel
    -- token encountered along the way (if any).
    CancelPath (Seq (TVar CancelState))

emptyCancelPath :: CancelPath
emptyCancelPath =
  CancelPath Seq.empty

-- | Extend a cancel path with a node.
cancelPathPush :: TVar CancelState -> CancelPath -> CancelPath
cancelPathPush var (CancelPath path) =
  CancelPath (path Seq.|> var)

-- | Eliminate the latest node that was pushed, if any.
cancelPathPop :: CancelPath -> CancelPath
cancelPathPop (CancelPath path) =
  case Seq.viewr path of
    Seq.EmptyR -> CancelPath path
    vars Seq.:> _ -> CancelPath vars

-- | Cancel the leaf of a path with a cancel token. If the leaf was previously cancelled directly, the cancel token is
-- ignored. This action never blocks.
cancelPathCancel :: CancelPath -> CancelToken -> STM ()
cancelPathCancel (CancelPath path) token =
  case Seq.viewr path of
    Seq.EmptyR -> pure ()
    _ Seq.:> var ->
      readTVar var >>= \case
        CancelState'Cancelled _token -> pure ()
        CancelState'NotCancelled -> writeTVar var (CancelState'Cancelled token)

-- | Block until the any node in path is cancelled, and return the cancel token with the widest scope that cancelled it.
--
-- For example, the path
--
-- @
-- [NotCancelled, Cancelled A, NotCancelled, Cancelled B]
--      ^                                        ^
--    root                                      leaf
-- @
--
-- would return
--
-- @
-- Cancelled A
-- @
cancelPathCancelled :: CancelPath -> STM CancelToken
cancelPathCancelled (CancelPath path) =
  foldr step retry path
  where
    step :: TVar CancelState -> STM CancelToken -> STM CancelToken
    step var action =
      readTVar var >>= \case
        CancelState'Cancelled token -> pure token
        CancelState'NotCancelled -> action

-- | Block until the leaf of a path is cancelled.
cancelPathLeafCancelled :: CancelPath -> STM CancelToken
cancelPathLeafCancelled (CancelPath path) =
  case Seq.viewr path of
    Seq.EmptyR -> retry
    _ Seq.:> var ->
      readTVar var >>= \case
        CancelState'Cancelled token -> pure token
        CancelState'NotCancelled -> retry
