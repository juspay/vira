{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub Checks API client

Implements GitHub App authentication and Checks API for reporting build status.

= Authentication Flow

1. Create JWT from App ID + Private Key (valid for 10 minutes)
2. Exchange JWT for Installation Access Token (valid for 1 hour)
3. Use Installation Access Token for Checks API calls

= Usage

@
result <- createCheckRun appSettings installationId repoOwner repoName headSha "Vira CI"
case result of
  Left err -> log Error $ "Failed to create check run: " <> show err
  Right checkRunId -> log Info $ "Created check run: " <> show checkRunId
@
-}
module Vira.Webhook.GitHub.ChecksAPI (
  -- * Types
  CheckRunStatus (..),
  CheckRunConclusion (..),
  CreateCheckRunRequest (..),
  CreateCheckRunResponse (..),
  InstallationToken (..),
  ChecksAPIError (..),

  -- * Functions
  createCheckRun,
  createJWT,
  getInstallationToken,
) where

import Control.Exception (try)
import Control.Lens ((?~))
import Crypto.JOSE (Error, JWK, fromRSA)
import Crypto.JOSE.Compact (encodeCompact)
import Crypto.JOSE.JWA.JWS (Alg (RS256))
import Crypto.JOSE.JWS (newJWSHeader)
import Crypto.JWT (
  NumericDate (..),
  SignedJWT,
  claimExp,
  claimIat,
  claimIss,
  emptyClaimsSet,
  runJOSE,
  signClaims,
 )
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.X509 (PrivKey (..))
import Data.X509.Memory (readKeyFileFromMemory)
import Effectful (Eff, IOE, (:>))
import Network.HTTP.Req (
  HttpException,
  POST (POST),
  ReqBodyJson (ReqBodyJson),
  defaultHttpConfig,
  header,
  https,
  jsonResponse,
  req,
  responseBody,
  runReq,
  (/:),
 )
import Vira.App.CLI (GitHubAppSettings (..))

-- | Status of a check run
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

-- | Conclusion of a completed check run
data CheckRunConclusion
  = ActionRequired
  | Cancelled
  | Failure
  | Neutral
  | Success
  | Skipped
  | Stale
  | TimedOut
  deriving stock (Show, Eq)

instance ToJSON CheckRunConclusion where
  toJSON = \case
    ActionRequired -> "action_required"
    Cancelled -> "cancelled"
    Failure -> "failure"
    Neutral -> "neutral"
    Success -> "success"
    Skipped -> "skipped"
    Stale -> "stale"
    TimedOut -> "timed_out"

-- | Request body for creating a check run
data CreateCheckRunRequest = CreateCheckRunRequest
  { name :: Text
  -- ^ Name of the check
  , headSha :: Text
  -- ^ SHA of the commit to attach the check to
  , status :: CheckRunStatus
  -- ^ Status of the check run
  , externalId :: Maybe Text
  -- ^ Optional reference for external system
  , detailsUrl :: Maybe Text
  -- ^ URL with details about the check run
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CreateCheckRunRequest where
  toJSON r =
    Aeson.object $
      catMaybes
        [ Just $ "name" .= r.name
        , Just $ "head_sha" .= r.headSha
        , Just $ "status" .= r.status
        , ("external_id" .=) <$> r.externalId
        , ("details_url" .=) <$> r.detailsUrl
        ]

-- | Response from creating a check run
newtype CreateCheckRunResponse = CreateCheckRunResponse
  { id :: Int
  -- ^ ID of the created check run
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Installation access token from GitHub
data InstallationToken = InstallationToken
  { token :: Text
  , expiresAt :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON InstallationToken where
  parseJSON = Aeson.withObject "InstallationToken" $ \v ->
    InstallationToken
      <$> v .: "token"
      <*> v .: "expires_at"

-- | Errors that can occur when using the Checks API
data ChecksAPIError
  = JWTCreationError Text
  | TokenExchangeError Text
  | CheckRunCreationError Text
  | PrivateKeyError Text
  | HttpError HttpException
  deriving stock (Show)

-- | Read RSA private key from PEM file and convert to JWK
readPrivateKey :: FilePath -> IO (Either ChecksAPIError JWK)
readPrivateKey path = do
  content <- readFileBS path
  case readKeyFileFromMemory content of
    [PrivKeyRSA rsaKey] -> pure $ Right $ fromRSA rsaKey
    [] -> pure $ Left $ PrivateKeyError "No RSA key found in PEM file"
    _ -> pure $ Left $ PrivateKeyError "Multiple keys or non-RSA key in PEM file"

-- | Create a JWT for GitHub App authentication
createJWT :: GitHubAppSettings -> IO (Either ChecksAPIError LBS.ByteString)
createJWT settings = do
  now <- getCurrentTime
  keyResult <- readPrivateKey settings.privateKeyPath
  case keyResult of
    Left err -> pure $ Left err
    Right jwk -> do
      -- Truncate to whole seconds to avoid scientific notation in JSON
      -- GitHub requires integer Unix timestamps, not floats
      let nowSeconds = truncateToSeconds now
          expSeconds = truncateToSeconds $ addUTCTime 600 now
          claims =
            emptyClaimsSet
              & claimIat
              ?~ NumericDate nowSeconds
                & claimExp
              ?~ NumericDate expSeconds
                & claimIss
              ?~ fromString (show settings.appId)
      result :: Either Error SignedJWT <- runJOSE $ signClaims jwk (newJWSHeader ((), RS256)) claims
      case result of
        Left err -> pure $ Left $ JWTCreationError $ show err
        Right signed -> pure $ Right $ encodeCompact signed
  where
    -- Truncate UTCTime to whole seconds (removes fractional seconds)
    truncateToSeconds t =
      posixSecondsToUTCTime $ fromIntegral (floor (utcTimeToPOSIXSeconds t) :: Integer)

-- | Exchange JWT for an installation access token
getInstallationToken ::
  (IOE :> es) =>
  LBS.ByteString ->
  Int ->
  Eff es (Either ChecksAPIError InstallationToken)
getInstallationToken jwt installationId = do
  result <- liftIO $ try @HttpException $ runReq defaultHttpConfig $ do
    let url = https "api.github.com" /: "app" /: "installations" /: show installationId /: "access_tokens"
        authHeader = "Bearer " <> toStrict jwt
    resp <-
      req
        POST
        url
        (ReqBodyJson (Aeson.object []))
        jsonResponse
        ( header "Authorization" authHeader
            <> header "Accept" "application/vnd.github+json"
            <> header "User-Agent" "Vira-CI"
        )
    pure $ responseBody resp
  pure $ first HttpError result

-- | Create a check run on a pull request
createCheckRun ::
  (IOE :> es) =>
  Text ->
  Text ->
  Text ->
  CreateCheckRunRequest ->
  Eff es (Either ChecksAPIError CreateCheckRunResponse)
createCheckRun accessToken owner repo checkRunReq = do
  result <- liftIO $ try @HttpException $ runReq defaultHttpConfig $ do
    let url = https "api.github.com" /: "repos" /: owner /: repo /: "check-runs"
        authHeader = "Bearer " <> encodeUtf8 accessToken
    resp <-
      req
        POST
        url
        (ReqBodyJson checkRunReq)
        jsonResponse
        ( header "Authorization" authHeader
            <> header "Accept" "application/vnd.github+json"
            <> header "User-Agent" "Vira-CI"
        )
    pure $ responseBody resp
  pure $ first HttpError result
