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
  runGitHubAsApp,
  newAppAuth,
  InstallationAccessTokens,
  GitHubError (..),
  AppAuth (..),
) where

import Control.Exception (try)
import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (FromJSON)
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import GitHub.REST (GHEndpoint (..), GitHubSettings (..), Token (..))
import GitHub.REST qualified as GH
import GitHub.REST.Auth (getJWTToken)
import Vira.Lib.GitHub (AppId (..), InstallationAccessToken (..), InstallationId (..), createInstallationAccessTokenE)

data GitHub :: Effect where
  QueryGitHub :: (FromJSON a) => InstallationId -> GHEndpoint -> GitHub m a
  QueryGitHub_ :: InstallationId -> GHEndpoint -> GitHub m ()

type instance DispatchOf GitHub = 'Dynamic

makeEffect ''GitHub

data GitHubError
  = -- | Failed to fetch installation access token
    TokenFetchFailed Text
  | -- | HTTP request to GitHub failed
    APICallFailed Text
  deriving stock (Show, Eq)

type InstallationAccessTokens = TVar (Map InstallationId InstallationAccessToken)

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

-- | Buffer time before expiry to refresh token
tokenBuffer :: NominalDiffTime
tokenBuffer = 5 * 60 -- 5 minutes

{- | Run the GitHub effect as a GitHub App

Interprets the effect by authenticating as a GitHub App Installation.
Handles JWT signing and Installation Token rotation automatically.
-}
runGitHubAsApp ::
  (IOE :> es, Error GitHubError :> es) =>
  AppAuth ->
  Eff (GitHub : es) a ->
  Eff es a
runGitHubAsApp auth = interpret $ \_ -> \case
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

    toToken iat = AccessToken iat.iatToken

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
    Just iat | addUTCTime tokenBuffer now < iat.iatExpiresAt -> pure iat
    _ -> do
      jwt <- liftIO $ getJWTToken authPrivateKey (unAppId authAppId)
      iat <- fetch jwt instId
      let tokens' = Map.insert instId iat tokens
      liftIO $ atomically $ writeTVar authTokens tokens'
      pure iat
  where
    fetch ::
      (IOE :> es, Error GitHubError :> es) =>
      Token ->
      InstallationId ->
      Eff es InstallationAccessToken
    fetch jwt inst = do
      let settings = GitHubSettings (Just jwt) "vira" ""
      result <-
        liftIO $
          try @SomeException $
            GH.runGitHubT settings $
              GH.queryGitHub (createInstallationAccessTokenE inst)
      case result of
        Left err -> throwError $ TokenFetchFailed (show err)
        Right iat -> pure iat
