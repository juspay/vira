{-# LANGUAGE DuplicateRecordFields #-}

{- | GitHub API Types and Endpoints

Pure types and endpoint definitions for GitHub REST API.
Use with 'Vira.Effect.GitHub' for effectful operations.
-}
module Vira.Lib.GitHub (
  -- * Identifiers
  AppId (..),
  InstallationId (..),
  InstallationAccessToken (..),
  Owner (..),
  Repo (..),

  -- * Check Run
  CheckRunId (..),
  CheckRunResponse (..),
  NewCheckRun (..),
  UpdateCheckRun (..),
  CheckRunStatus (..),
  CheckRunConclusion (..),

  -- * Endpoints
  createCheckRunE,
  updateCheckRunE,
  createInstallationAccessTokenE,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:))
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import GitHub.REST (GHEndpoint (..), KeyValue (..))
import Network.HTTP.Types (StdMethod (..))

-- | GitHub App ID
newtype AppId = AppId {unAppId :: Int}
  deriving newtype (Show, Eq, Ord, Read)

-- | GitHub App Installation ID
newtype InstallationId = InstallationId Int
  deriving newtype (Show, Eq, Ord)

data InstallationAccessToken = InstallationAccessToken
  { iatToken :: ByteString
  , iatExpiresAt :: UTCTime
  }
  deriving stock (Show, Eq)

instance FromJSON InstallationAccessToken where
  parseJSON = withObject "InstallationAccessToken" $ \o -> do
    token <- o .: "token"
    expiresAt <- o .: "expires_at"
    utcTime <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" expiresAt
    pure
      InstallationAccessToken
        { iatToken = encodeUtf8 @Text token
        , iatExpiresAt = utcTime
        }

-- | Repository owner
newtype Owner = Owner Text
  deriving newtype (Show, Eq, ToJSON)

-- | Repository name
newtype Repo = Repo Text
  deriving newtype (Show, Eq, ToJSON)

-- | Check run ID returned by GitHub
newtype CheckRunId = CheckRunId {unCheckRunId :: Int}
  deriving newtype (Show, Eq, Ord, FromJSON, ToJSON)

-- | Response from creating a check run
newtype CheckRunResponse = CheckRunResponse
  { checkRunId :: CheckRunId
  }
  deriving stock (Show, Eq)

instance FromJSON CheckRunResponse where
  parseJSON = withObject "CheckRunResponse" $ \o ->
    CheckRunResponse <$> o .: "id"

-- | Check run status
data CheckRunStatus
  = Queued
  | InProgress
  | Completed
  deriving stock (Show, Eq)

instance ToText CheckRunStatus where
  toText = \case
    Queued -> "queued"
    InProgress -> "in_progress"
    Completed -> "completed"

-- | Check run conclusion (required when status is 'Completed')
data CheckRunConclusion
  = Success
  | Failure
  | Cancelled
  | TimedOut
  | ActionRequired
  | Skipped
  deriving stock (Show, Eq)

instance ToText CheckRunConclusion where
  toText = \case
    Success -> "success"
    Failure -> "failure"
    Cancelled -> "cancelled"
    TimedOut -> "timed_out"
    ActionRequired -> "action_required"
    Skipped -> "skipped"

-- | Request body for creating a check run
data NewCheckRun = NewCheckRun
  { name :: Text
  , headSha :: Text
  , status :: Maybe CheckRunStatus
  }
  deriving stock (Show, Eq)

-- | Request body for updating a check run
data UpdateCheckRun = UpdateCheckRun
  { status :: CheckRunStatus
  , conclusion :: Maybe CheckRunConclusion
  -- ^ Required when status is 'Completed'
  }
  deriving stock (Show, Eq)

---------------
-- Endpoints
---------------

createCheckRunE :: Owner -> Repo -> NewCheckRun -> GHEndpoint
createCheckRunE (Owner owner) (Repo repo) cr =
  GHEndpoint
    { method = POST
    , endpoint = "/repos/:owner/:repo/check-runs"
    , endpointVals = ["owner" := owner, "repo" := repo]
    , ghData =
        ["name" := cr.name, "head_sha" := cr.headSha]
          <> maybe [] (\s -> ["status" := toText s]) cr.status
    }

updateCheckRunE :: Owner -> Repo -> CheckRunId -> UpdateCheckRun -> GHEndpoint
updateCheckRunE (Owner owner) (Repo repo) (CheckRunId checkRunId) upd =
  GHEndpoint
    { method = PATCH
    , endpoint = "/repos/:owner/:repo/check-runs/:check_run_id"
    , endpointVals = ["owner" := owner, "repo" := repo, "check_run_id" := checkRunId]
    , ghData =
        ["status" := toText upd.status]
          <> maybe [] (\c -> ["conclusion" := toText c]) upd.conclusion
    }

createInstallationAccessTokenE :: InstallationId -> GHEndpoint
createInstallationAccessTokenE (InstallationId instId) =
  GHEndpoint
    { method = POST
    , endpoint = "/app/installations/:installation_id/access_tokens"
    , endpointVals = ["installation_id" := instId]
    , ghData = []
    }
