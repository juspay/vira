-- | Broadcast channel type for entity-scoped SSE events
module Vira.App.Broadcast.Type (
  BroadcastScope (..),
  UpdateBroadcast,
) where

import Control.Concurrent.STM (TChan)
import Effectful.Git (RepoName (..))
import Text.Show (Show (..))
import Vira.State.Type (JobId (..))

-- | Entity scope for broadcast events
data BroadcastScope
  = JobScope JobId
  | RepoScope RepoName
  deriving stock (Eq)

-- | Custom Show instance for SSE event names (using "/" for filepattern compatibility)
instance Show BroadcastScope where
  show (JobScope jobId) = "job/" <> Prelude.show jobId
  show (RepoScope (RepoName name)) = toString $ "repo/" <> name

-- | Broadcast channel for entity-scoped update events
type UpdateBroadcast = TChan BroadcastScope
