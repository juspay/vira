{-# LANGUAGE TemplateHaskell #-}

{- | GitHub Effect

Effectful interface for GitHub API operations.
Handles authentication and token caching internally.
-}
module Vira.Effect.GitHub (
  GitHub (..),
  queryGitHub,
  queryGitHub_,
  runGitHub,
  InstallationAccessTokens,
  GitHubError (..),
) where

-- import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (try)
import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (FromJSON, Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, defaultTimeLocale, getCurrentTime)
import Data.Time.Format (parseTimeM)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import GitHub.REST (GHEndpoint (..), GitHubSettings (..), KeyValue (..), StdMethod (..), Token (..))
import GitHub.REST qualified as GH
import GitHub.REST.Auth (getJWTToken)
import Vira.Lib.GitHub (AppId (..), InstallationId (..))

-- | Errors during GitHub operations
data GitHubError
  = TokenFetchFailed Text
  | APICallFailed Text
  deriving stock (Show, Eq)

type InstallationAccessTokens = TVar (Map InstallationId (Token, UTCTime))

-- | Buffer time before expiry to refresh token (5 minutes)
tokenBuffer :: NominalDiffTime
tokenBuffer = 5 * 60

-- | GitHub Effect
data GitHub :: Effect where
  QueryGitHub :: (FromJSON a) => InstallationId -> GHEndpoint -> GitHub m a
  QueryGitHub_ :: InstallationId -> GHEndpoint -> GitHub m ()

type instance DispatchOf GitHub = 'Dynamic

makeEffect ''GitHub

-- | Run GitHub effect with authentication
runGitHub ::
  (IOE :> es, Error GitHubError :> es) =>
  PrivateKey ->
  AppId ->
  InstallationAccessTokens ->
  Eff (GitHub : es) a ->
  Eff es a
runGitHub privateKey appId iats = interpret $ \_ -> \case
  QueryGitHub instId endpoint -> do
    token <- getValidToken privateKey appId iats instId
    runQuery token endpoint
  QueryGitHub_ instId endpoint -> do
    token <- getValidToken privateKey appId iats instId
    runQuery_ token endpoint

-- | Get valid token, fetching if needed
getValidToken ::
  (IOE :> es, Error GitHubError :> es) =>
  PrivateKey ->
  AppId ->
  InstallationAccessTokens ->
  InstallationId ->
  Eff es Token
getValidToken privateKey (AppId appId) iats instId = do
  now <- liftIO getCurrentTime
  cached <- liftIO $ readTVarIO iats

  case Map.lookup instId cached of
    Just (token, expiry)
      | addUTCTime tokenBuffer now < expiry -> pure token
    _ -> fetchAndCache cached
  where
    fetchAndCache cached = do
      jwt <- liftIO $ getJWTToken privateKey appId
      (token, expiry) <- fetchInstallationToken jwt instId
      let cached' = Map.insert instId (token, expiry) cached
      liftIO $ atomically $ writeTVar iats cached'
      pure token

-- | Fetch installation token from GitHub
fetchInstallationToken ::
  (IOE :> es, Error GitHubError :> es) =>
  Token ->
  InstallationId ->
  Eff es (Token, UTCTime)
fetchInstallationToken jwt (InstallationId instId) = do
  let settings = GitHubSettings (Just jwt) "vira" ""
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
    Right value -> case parseTokenResponse value of
      Nothing -> throwError $ TokenFetchFailed "Failed to parse token response"
      Just tokenExpiry -> pure tokenExpiry

{- | Parse token response
TODO: offload to a library
-}
parseTokenResponse :: Value -> Maybe (Token, UTCTime)
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

-- | Run query with typed result
runQuery ::
  (IOE :> es, Error GitHubError :> es, FromJSON a) =>
  Token ->
  GHEndpoint ->
  Eff es a
runQuery token endpoint = do
  let settings = GitHubSettings (Just (toAccessToken token)) "vira" ""
  result <- liftIO $ try @SomeException $ GH.runGitHubT settings $ GH.queryGitHub endpoint
  case result of
    Left err -> throwError $ APICallFailed (show err)
    Right value -> pure value
  where
    toAccessToken (AccessToken bs) = AccessToken bs
    toAccessToken (BearerToken bs) = AccessToken bs -- Convert JWT to access token format

-- | Run query ignoring result
runQuery_ ::
  (IOE :> es, Error GitHubError :> es) =>
  Token ->
  GHEndpoint ->
  Eff es ()
runQuery_ token endpoint = do
  let settings = GitHubSettings (Just (toAccessToken token)) "vira" ""
  result <- liftIO $ try @SomeException $ GH.runGitHubT settings $ GH.queryGitHub_ endpoint
  case result of
    Left err -> throwError $ APICallFailed (show err)
    Right _ -> pass
  where
    toAccessToken (AccessToken bs) = AccessToken bs
    toAccessToken (BearerToken bs) = AccessToken bs
