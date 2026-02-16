{- | GitHub API Types and Endpoints

Pure types and endpoint definitions for GitHub REST API.
Use with 'Vira.GitHub.Effect' for effectful operations.
-}
module Vira.Lib.GitHub (
  -- * Identifiers
  AppId (..),
  InstallationId (..),
  Owner (..),
  Repo (..),

  -- * Check Run
  NewCheckRun (..),
  CheckRunStatus (..),
  createCheckRunE,
) where

import Data.Aeson (ToJSON (..))
import GitHub.REST (GHEndpoint (..), KeyValue (..))
import Network.HTTP.Types (StdMethod (..))

-- | GitHub App ID
newtype AppId = AppId Int
  deriving newtype (Show, Eq, Ord)

-- | GitHub App Installation ID
newtype InstallationId = InstallationId Int
  deriving newtype (Show, Eq, Ord)

-- | Repository owner
newtype Owner = Owner Text
  deriving newtype (Show, Eq, ToJSON)

-- | Repository name
newtype Repo = Repo Text
  deriving newtype (Show, Eq, ToJSON)

-- | Check run status
data CheckRunStatus
  = Queued
  | InProgress
  | Completed
  deriving stock (Show, Eq)

-- | Request body for creating a check run
data NewCheckRun = NewCheckRun
  { name :: Text
  , headSha :: Text
  , status :: Maybe CheckRunStatus
  }
  deriving stock (Show, Eq)

-- | Create a check run
createCheckRunE :: Owner -> Repo -> NewCheckRun -> GHEndpoint
createCheckRunE (Owner owner) (Repo repo) cr =
  GHEndpoint
    { method = POST
    , endpoint = "/repos/:owner/:repo/check-runs"
    , endpointVals = ["owner" := owner, "repo" := repo]
    , ghData =
        ["name" := cr.name, "head_sha" := cr.headSha]
          <> maybe [] (\s -> ["status" := statusText s]) cr.status
    }
  where
    statusText = \case
      Queued -> "queued" :: Text
      InProgress -> "in_progress"
      Completed -> "completed"
