{- | GitHub API

GitHub App authentication and API operations using github-rest.
Handles JWT creation, installation token caching, and API endpoints.
-}
module Vira.Lib.GitHub (
  -- * Types
  AppId (..),
  InstallationId (..),
  Expiry,
  GitHubError (..),
  Owner (..),
  Repo (..),
  NewCheckRun (..),
  CheckRunStatus (..),
  InstallationAccessTokens,

  -- * Token Management
  getValidIAT,

  -- * Endpoints
  createCheckRunE,
) where

import Control.Exception (try)
import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Map qualified as Map
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, getCurrentTime)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format (parseTimeM)
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import GitHub.REST (GHEndpoint (..), GitHubData, KeyValue (..), Token (..))
import GitHub.REST qualified as GH
import GitHub.REST.Auth (getJWTToken)
import Network.HTTP.Types (StdMethod (..))

-- | GitHub App ID
newtype AppId = AppId Int
  deriving newtype (Show, Eq, Ord)

-- | GitHub App Installation ID
newtype InstallationId = InstallationId Int
  deriving newtype (Show, Eq, Ord)

type Expiry = UTCTime

data GitHubError
  = JWTCreationFailed Text
  | TokenFetchFailed Text
  deriving stock (Show, Eq)

-- | Repository owner
newtype Owner = Owner Text
  deriving newtype (Show, Eq, ToJSON)

-- | Repository name
newtype Repo = Repo Text
  deriving newtype (Show, Eq, ToJSON)

data CheckRunStatus
  = Queued
  | InProgress
  | Completed
  deriving stock (Show, Eq)

instance ToJSON CheckRunStatus where
  toJSON = \case
    Queued -> "queued"
    InProgress -> "in_progress"
    Completed -> "completed"

-- | Request body for creating a check run
data NewCheckRun = NewCheckRun
  { name :: Text
  , headSha :: Text
  , status :: Maybe CheckRunStatus
  }
  deriving stock (Show, Eq)

instance ToJSON NewCheckRun where
  toJSON cr =
    Aeson.object $
      [ "name" .= cr.name
      , "head_sha" .= cr.headSha
      ]
        <> maybe [] (\s -> ["status" .= s]) cr.status

-- | Buffer time before expiry to refresh token (5 minutes)
tokenBuffer :: NominalDiffTime
tokenBuffer = 5 * 60

type InstallationAccessTokens = TVar (Map InstallationId (Token, Expiry))

{- | Get a valid Installation Access Token, using cache if available.

Creates JWT internally, checks cache for valid token, fetches new one if needed.
-- | Check run status
-}
getValidIAT ::
  (IOE :> es, Error GitHubError :> es) =>
  InstallationAccessTokens ->
  PrivateKey ->
  AppId ->
  InstallationId ->
  Eff es Token
getValidIAT cache privateKey (AppId appId) installationId = do
  now <- liftIO getCurrentTime
  cached <- liftIO $ readTVarIO cache

  case Map.lookup installationId cached of
    Just (token, expiry)
      | addUTCTime tokenBuffer now < expiry -> pure token
    _ -> fetchAndCache cached
  where
    fetchAndCache cached = do
      -- Create JWT for app authentication
      jwt <- liftIO $ getJWTToken privateKey appId

      -- Fetch installation token
      (token, expiry) <- fetchInstallationToken jwt installationId

      -- Update cache
      let cached' = Map.insert installationId (token, expiry) cached
      liftIO $ atomically $ writeTVar cache cached'

      pure token

-- | Fetch installation access token from GitHub API
fetchInstallationToken ::
  (IOE :> es, Error GitHubError :> es) =>
  Token ->
  InstallationId ->
  Eff es (Token, Expiry)
fetchInstallationToken jwt (InstallationId instId) = do
  let settings = GH.GitHubSettings (Just jwt) "vira" ""
      endpoint =
        GHEndpoint
          { method = POST
          , endpoint = "/app/installations/:installation_id/access_tokens"
          , endpointVals = ["installation_id" := instId]
          , ghData = []
          }

  result <- liftIO $ try @SomeException $ GH.runGitHubT settings $ GH.queryGitHub endpoint

  case result of
    Left err -> throwError $ TokenFetchFailed (show err)
    Right value -> do
      case parseTokenResponse value of
        Nothing -> throwError $ TokenFetchFailed "Failed to parse token response"
        Just tokenExpiry -> pure tokenExpiry

-- | Parse token and expiry from GitHub API response
parseTokenResponse :: Aeson.Value -> Maybe (Token, Expiry)
parseTokenResponse value = do
  obj <- case value of
    Aeson.Object o -> Just o
    _ -> Nothing
  tokenText <- case AesonKM.lookup "token" obj of
    Just (Aeson.String t) -> Just t
    _ -> Nothing
  expiryText <- case AesonKM.lookup "expires_at" obj of
    Just (Aeson.String t) -> Just t
    _ -> Nothing
  expiry <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toString expiryText)
  pure (AccessToken (encodeUtf8 tokenText), expiry)

-- | Create a check run endpoint
createCheckRunE :: Owner -> Repo -> NewCheckRun -> GHEndpoint
createCheckRunE (Owner owner) (Repo repo) checkRun =
  GHEndpoint
    { method = POST
    , endpoint = "/repos/:owner/:repo/check-runs"
    , endpointVals = ["owner" := owner, "repo" := repo]
    , ghData = toGitHubData checkRun
    }

-- | Convert NewCheckRun to GitHubData (key-value pairs)
toGitHubData :: NewCheckRun -> GitHubData
toGitHubData cr =
  ["name" := cr.name, "head_sha" := cr.headSha]
    <> maybe [] (\s -> ["status" := statusText s]) cr.status
  where
    statusText = \case
      Queued -> "queued" :: Text
      InProgress -> "in_progress"
      Completed -> "completed"
