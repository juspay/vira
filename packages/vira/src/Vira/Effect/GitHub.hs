{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | GitHub Effect

Effectful interface for GitHub API operations as a GitHub app installation.
Handles authentication and token caching.
-}
module Vira.Effect.GitHub (
  GitHub (..),
  queryGitHub,
  queryGitHub_,
  runGitHub,
  newAppAuth,
  InstallationAccessTokens,
  GitHubError (..),
  AppAuth (..),
) where

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
import GitHub.REST (GHEndpoint (..), GitHubSettings (..), Token (..))
import GitHub.REST qualified as GH
import GitHub.REST.Auth (getJWTToken)
import Vira.Lib.GitHub (AppId (..), InstallationAccessToken (..), InstallationId (..), createInstallationAccessTokenE)

-- | Errors during GitHub operations
data GitHubError
  = TokenFetchFailed Text
  | APICallFailed Text
  deriving stock (Show, Eq)

type InstallationAccessTokens = TVar (Map InstallationId (InstallationAccessToken, UTCTime))

data AppAuth = AppAuth
  { authPrivateKey :: PrivateKey
  , authAppId :: AppId
  , authTokens :: InstallationAccessTokens
  }

newAppAuth :: PrivateKey -> AppId -> IO AppAuth
newAppAuth key appId = do
  tokens <- newTVarIO Map.empty
  pure $
    AppAuth
      { authPrivateKey = key
      , authAppId = appId
      , authTokens = tokens
      }

-- | Buffer time before expiry to refresh token (5 minutes)
tokenBuffer :: NominalDiffTime
tokenBuffer = 5 * 60

data GitHub :: Effect where
  QueryGitHub :: (FromJSON a) => InstallationId -> GHEndpoint -> GitHub m a
  QueryGitHub_ :: InstallationId -> GHEndpoint -> GitHub m ()

type instance DispatchOf GitHub = 'Dynamic

makeEffect ''GitHub

{- | Run the GitHub effect as a GitHub App
Interprets the effect by authenticating as a GitHub App Installation.
Handles JWT signing and Installation Token rotation automatically.
-}
runGitHub ::
  (IOE :> es, Error GitHubError :> es) =>
  AppAuth ->
  Eff (GitHub : es) a ->
  Eff es a
runGitHub auth = interpret $ \_ -> \case
  QueryGitHub instId endpoint -> do
    token <- getValidToken auth instId
    executeRequest token (GH.queryGitHub endpoint)
  QueryGitHub_ instId endpoint -> do
    token <- getValidToken auth instId
    executeRequest token (GH.queryGitHub_ endpoint)
  where
    executeRequest token action = do
      let settings = GitHubSettings (Just (toToken token)) "vira" ""
      result <- liftIO $ try @SomeException $ GH.runGitHubT settings action
      case result of
        Left err -> throwError $ APICallFailed (show err)
        Right val -> pure val

    toToken (InstallationAccessToken iat) = AccessToken iat

-- | Get valid github app installation access token, fetching if needed
getValidToken ::
  (IOE :> es, Error GitHubError :> es) =>
  AppAuth ->
  InstallationId ->
  Eff es InstallationAccessToken
getValidToken AppAuth {..} instId = do
  now <- liftIO getCurrentTime
  tokens <- liftIO $ readTVarIO authTokens

  case Map.lookup instId tokens of
    Just (token, expiry)
      | addUTCTime tokenBuffer now < expiry -> pure token
    _ -> fetchAndCache tokens
  where
    fetchAndCache tokens = do
      jwt <- liftIO $ getJWTToken authPrivateKey (unAppId authAppId)
      (token, expiry) <- fetch jwt instId
      let tokens' = Map.insert instId (token, expiry) tokens
      liftIO $ atomically $ writeTVar authTokens tokens'
      pure token

    fetch ::
      (IOE :> es, Error GitHubError :> es) =>
      Token ->
      InstallationId ->
      Eff es (InstallationAccessToken, UTCTime)
    fetch jwt inst = do
      let settings = GitHubSettings (Just jwt) "vira" ""
      result <-
        liftIO $
          try @SomeException $
            GH.runGitHubT settings $
              GH.queryGitHub (createInstallationAccessTokenE inst)
      case result of
        Left err -> throwError $ TokenFetchFailed (show err)
        Right value -> case parseTokenResponse value of
          Nothing -> throwError $ TokenFetchFailed "Failed to parse token response"
          Just tokenExpiry -> pure tokenExpiry

{- | Parse token response
TODO: offload to a library
-}
parseTokenResponse :: Value -> Maybe (InstallationAccessToken, UTCTime)
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
  pure (InstallationAccessToken (encodeUtf8 tokenText), expiry)
