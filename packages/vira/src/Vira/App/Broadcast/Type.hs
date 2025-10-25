{-# LANGUAGE DeriveAnyClass #-}

-- | Broadcast channel type for entity-scoped SSE events
module Vira.App.Broadcast.Type (
  BroadcastScope (..),
  UpdateBroadcast,
  matchesAnyScope,
  parseScopes,
) where

import Control.Concurrent.STM (TChan)
import Data.Aeson (FromJSON, ToJSON, decode)
import Effectful.Git (RepoName)
import Vira.State.Type (JobId)

{- | Entity scope for broadcast events and patterns

This type serves dual purposes:
1. Broadcasting: Sent when an entity changes (always uses specific IDs with Just)
2. Pattern matching: Subscriptions that filter broadcasts (can use Nothing for wildcards)

Examples:
- @JobScope (Just 123)@ - Specific job broadcast or pattern
- @JobScope Nothing@ - Wildcard pattern matching all jobs (used on home page)
- @RepoScope "my-repo"@ - Specific repo broadcast or pattern (no wildcard needed)
-}
data BroadcastScope
  = -- | Job-scoped events. Nothing matches all jobs, Just matches specific job.
    JobScope (Maybe JobId)
  | -- | Repo-scoped events. Always matches specific repository.
    RepoScope RepoName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Broadcast channel for entity-scoped update events
type UpdateBroadcast = TChan BroadcastScope

-- | Parse JSON-encoded list of scopes
parseScopes :: Text -> [BroadcastScope]
parseScopes = fromMaybe [] . decode . encodeUtf8

{- | Check if a broadcast matches any of the patterns

Matching rules:
- @JobScope Nothing@ (wildcard) matches any @JobScope (Just _)@ broadcast
- All other patterns require exact equality
-}
matchesAnyScope :: [BroadcastScope] -> BroadcastScope -> Bool
matchesAnyScope patterns broadcast =
  any (`matches` broadcast) patterns
  where
    -- JobScope Nothing is a wildcard matching any job
    matches (JobScope Nothing) (JobScope _) = True
    matches pat scope = pat == scope
