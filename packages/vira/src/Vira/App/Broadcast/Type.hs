-- | Broadcast channel type for entity-scoped SSE events
module Vira.App.Broadcast.Type (
  BroadcastScope (..),
  UpdateBroadcast,
  ScopePattern,
  matchesAnyPattern,
  parseScopePatterns,
) where

import Control.Concurrent.STM (TChan)
import Data.Text (splitOn, strip)
import Effectful.Git (RepoName (..))
import System.FilePath ((</>))
import System.FilePattern (FilePattern, (?==))
import Text.Show qualified
import Vira.State.Type (JobId (..))

-- | Entity scope for broadcast events
data BroadcastScope
  = JobScope JobId
  | RepoScope RepoName
  deriving stock (Eq)

-- | Custom Show instance for SSE event names (using "/" for filepattern compatibility)
instance Show BroadcastScope where
  show (JobScope jobId) = "job" </> Text.Show.show jobId
  show (RepoScope (RepoName name)) = "repo" </> toString name

-- | Broadcast channel for entity-scoped update events
type UpdateBroadcast = TChan BroadcastScope

-- | Pattern for matching broadcast scopes (filepattern glob)
type ScopePattern = FilePattern

-- | Parse comma-separated scope patterns
parseScopePatterns :: Text -> [ScopePattern]
parseScopePatterns = map (toString . strip) . splitOn ","

-- | Check if a scope matches any of the patterns (using filepattern globbing)
matchesAnyPattern :: [ScopePattern] -> BroadcastScope -> Bool
matchesAnyPattern patterns scope =
  any (?== show scope) patterns
