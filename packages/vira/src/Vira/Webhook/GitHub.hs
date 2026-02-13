{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub App webhook handler as WAI sub-app

This module provides a standalone WAI application for handling GitHub webhook events.
It is decoupled from the main Vira web server and can be mounted as middleware.
-}
module Vira.Webhook.GitHub (
  -- * WAI Middleware
  webhookMiddleware,
) where

import Colog (Severity (..))
import Crypto.PubKey.RSA.Read (ReadRsaKeyError)
import Effectful (Eff, IOE)
import Effectful qualified as Eff
import Effectful.Colog.Simple (log)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Error.Static qualified as Eff
import GitHub.Auth.App (AppAuthError)
import GitHub.Data.Webhooks.Events (
  PullRequestEvent (..),
  PushEvent,
 )
import GitHub.Data.Webhooks.Payload (HookPullRequest (..), HookRepository (..), HookSimpleUser (..), HookUser (..), PullRequestTarget (..))
import GitHub.REST (GitHubSettings (..), queryGitHub_, runGitHubT)
import Network.Wai (Middleware, pathInfo)
import Servant
import Servant.GitHub.Webhook (GitHubEvent, GitHubKey (..))
import Servant.Server.Generic (AsServer, genericServeTWithContext)
import Vira.App (GlobalSettings, ViraRuntimeState)
import Vira.App.CLI (GHAppAuthSettings (..), WebSettings (..))
import Vira.Lib.Crypto (readRsaPem)
import Vira.Lib.GitHub (AppId (..), CheckRunStatus (..), GitHubError (..), InstallationAccessTokens, InstallationId (..), NewCheckRun (..), Owner (..), Repo (..), createCheckRunE, getValidIAT)
import Vira.Web.Stack (AppServantStack, runAppInServant)

-- | Errors that can occur when creating a check run
data CheckRunError
  = NoInstallationId
  | AuthError AppAuthError
  | CheckRunCreationError Text
  | InvalidPrivateKey ReadRsaKeyError
  deriving stock (Show)

-- | API type for GitHub webhook events
data Routes mode = Routes
  { _pr :: mode :- GitHubEvent PullRequestEvent :> Post '[JSON] NoContent
  , _push :: mode :- GitHubEvent PushEvent :> Post '[JSON] NoContent
  }
  deriving stock (Generic)

-- | Servant handlers for GitHub webhook events
handlers :: InstallationAccessTokens -> GlobalSettings -> ViraRuntimeState -> WebSettings -> Routes AsServer
handlers iats globalSettings viraRuntimeState webSettings =
  Routes
    { _pr = runAppInServant globalSettings viraRuntimeState webSettings . prHandler iats webSettings
    , _push = runAppInServant globalSettings viraRuntimeState webSettings . pushHandler
    }

-- | Handle pull request events by creating a GitHub Check run
prHandler :: InstallationAccessTokens -> WebSettings -> PullRequestEvent -> Eff AppServantStack NoContent
prHandler iats webSettings event = do
  log Info $ "Received PR on github:" <> show event

  case webSettings.ghAppAuthSettings of
    Just ghAppSettings -> do
      result <- runErrorNoCallStack @GitHubError $ createCheckRun iats ghAppSettings event
      case result of
        Left err -> log Warning (show err)
        Right _ -> log Info "Check run created successfully"
    Nothing -> log Warning "GitHub App settings not configured, skipping check run creation"
  pure NoContent

-- | Create a check run for a pull request event
createCheckRun ::
  (IOE Eff.:> es, Eff.Error GitHubError Eff.:> es) =>
  InstallationAccessTokens ->
  GHAppAuthSettings ->
  PullRequestEvent ->
  Eff es ()
createCheckRun iats appAuth prEvent = do
  let repo = evPullReqRepo prEvent
      repoName = whRepoName repo
      repoOwner = getOwnerLogin $ whRepoOwner repo
      prTarget = whPullReqHead $ evPullReqPayload prEvent
      checkRun = mkNewCheckRun prTarget

  case evPullReqInstallationId prEvent of
    Nothing -> Eff.throwError $ TokenFetchFailed "No installation ID in event"
    Just instId -> do
      -- Read private key
      rsaPem <- liftIO $ readFileBS appAuth.privateKeyPath
      privateKey <- readRsaPem rsaPem

      -- Get valid installation token (cached)
      token <- getValidIAT iats privateKey (AppId appAuth.appId) (InstallationId instId)

      -- Execute check run creation
      let settings = GitHubSettings (Just token) "vira" ""
      runGitHubT settings $
        queryGitHub_ (createCheckRunE (Owner repoOwner) (Repo repoName) checkRun)
  where
    getOwnerLogin :: Either HookSimpleUser HookUser -> Text
    getOwnerLogin = either (fromMaybe "" . whSimplUserLogin) whUserLogin

    mkNewCheckRun :: PullRequestTarget -> NewCheckRun
    mkNewCheckRun prTarget =
      NewCheckRun
        { name = "Vira CI"
        , headSha = whPullReqTargetSha prTarget
        , status = Just InProgress
        }

pushHandler :: PushEvent -> Eff AppServantStack NoContent
pushHandler _ = do
  log Info "Received Push"
  pure NoContent

{- | WAI middleware that mounts the GitHub webhook at @\/webhook\/github@

The webhook initializes its own Servant context with the GitHub secret key,
decoupled from the main Vira server's Servant context.

Usage:

@
let middleware = webhookMiddleware globalSettings viraRuntimeState webSettings
    app = middleware mainApp
@
-}
webhookMiddleware :: InstallationAccessTokens -> GlobalSettings -> ViraRuntimeState -> WebSettings -> Middleware
webhookMiddleware iats globalSettings viraRuntimeState webSettings app req sendResponse =
  case pathInfo req of
    ("webhook" : "github" : rest) -> do
      -- Route to webhook sub-app with path stripped
      let req' = req {pathInfo = rest}
      webhookApp req' sendResponse
    _ -> app req sendResponse
  where
    key = maybe mempty encodeUtf8 webSettings.ghWebhookSecret
    githubKey = GitHubKey $ pure key
    webhookApp =
      genericServeTWithContext
        id
        (handlers iats globalSettings viraRuntimeState webSettings)
        (githubKey :. EmptyContext)
