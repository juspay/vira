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
import Effectful (Eff)
import Effectful.Colog.Simple (log)
import GitHub (executeRequest)
import GitHub.Auth (Auth (..))
import GitHub.Data.Definitions (Owner)
import GitHub.Data.Name (mkName)
import GitHub.Data.Repos (Repo)
import GitHub.Data.Webhooks.Events (
  PullRequestEvent (..),
  PushEvent,
 )
import GitHub.Data.Webhooks.Payload (
  HookPullRequest (..),
  HookRepository (..),
  HookSimpleUser (..),
  HookUser (..),
  PullRequestTarget (..),
 )
import GitHub.Endpoints.Checks (
  CheckRunStatus (..),
  NewCheckRun (..),
  createCheckRunR,
 )
import Network.Wai (Middleware, pathInfo)
import Servant
import Servant.GitHub.Webhook (GitHubEvent, GitHubKey (..))
import Servant.Server.Generic (AsServer, genericServeTWithContext)
import Vira.App (GlobalSettings, ViraRuntimeState)
import Vira.App.CLI (GitHubAppSettings (..), WebSettings (..))
import Vira.Web.Stack (AppServantStack, runAppInServant)
import Vira.Webhook.GitHub.ChecksAPI (
  ChecksAPIError (..),
  InstallationToken (..),
  createJWT,
  getInstallationToken,
 )

-- | API type for GitHub webhook events
data Routes mode = Routes
  { _pr :: mode :- GitHubEvent PullRequestEvent :> Post '[JSON] NoContent
  , _push :: mode :- GitHubEvent PushEvent :> Post '[JSON] NoContent
  }
  deriving stock (Generic)

-- | Servant handlers for GitHub webhook events
handlers :: GlobalSettings -> ViraRuntimeState -> WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings =
  Routes
    { _pr = runAppInServant globalSettings viraRuntimeState webSettings . prHandler webSettings
    , _push = runAppInServant globalSettings viraRuntimeState webSettings . pushHandler
    }

-- | Handle pull request events by creating a GitHub Check run
prHandler :: WebSettings -> PullRequestEvent -> Eff AppServantStack NoContent
prHandler webSettings event = do
  -- Extract repo owner and name
  let repo = evPullReqRepo event
      repoName = whRepoName repo
      repoOwner = getOwnerLogin $ whRepoOwner repo

      -- Extract head SHA from the pull request
      pr = evPullReqPayload event
      headSha = whPullReqTargetSha $ whPullReqHead pr

      -- Get installation ID from the event
      maybeInstallationId = evPullReqInstallationId event

  log Info $ "Received PR on github:" <> repoOwner <> "/" <> repoName <> " at " <> headSha

  -- Create check run if GitHub App is configured
  case (webSettings.githubApp, maybeInstallationId) of
    (Just appSettings, Just installationId) -> do
      result <- createCheckRunForPR appSettings installationId repoOwner repoName headSha
      case result of
        Left err -> log Error $ "Failed to create check run: " <> show err
        Right _ -> log Info "Created check run successfully"
    (Nothing, _) -> log Warning "GitHub App not configured, skipping check run creation"
    (_, Nothing) -> log Warning "No installation ID in event, skipping check run creation"

  pure NoContent
  where
    getOwnerLogin :: Either HookSimpleUser HookUser -> Text
    getOwnerLogin = either (fromMaybe "" . whSimplUserLogin) whUserLogin

-- | Create a check run for a pull request
createCheckRunForPR ::
  GitHubAppSettings ->
  Int ->
  Text ->
  Text ->
  Text ->
  Eff AppServantStack (Either ChecksAPIError ())
createCheckRunForPR appSettings installationId owner repo headSha = do
  -- Create JWT for GitHub App authentication
  jwtResult <- liftIO $ createJWT appSettings
  case jwtResult of
    Left err -> pure $ Left err
    Right jwt -> do
      -- Exchange JWT for installation access token
      tokenResult <- getInstallationToken jwt installationId
      case tokenResult of
        Left err -> pure $ Left err
        Right installationToken -> do
          -- Create check run with "in_progress" status
          let checkReq =
                NewCheckRun
                  { newCheckRunName = "Vira CI"
                  , newCheckRunHeadSha = headSha
                  , newCheckRunStatus = Just CheckRunInProgress
                  , newCheckRunExternalId = Nothing
                  , newCheckRunDetailsUrl = Nothing -- TODO: Add URL to Vira job page
                  , newCheckRunStartedAt = Nothing
                  , newCheckRunConclusion = Nothing
                  , newCheckRunCompletedAt = Nothing
                  , newCheckRunOutput = Nothing
                  , newCheckRunActions = Nothing
                  }
              req = createCheckRunR (mkName (Proxy @Owner) owner) (mkName (Proxy @Repo) repo) checkReq
              auth = OAuth $ encodeUtf8 installationToken.installationTokenToken
          checkResult <- liftIO $ executeRequest auth req
          pure $ first (CheckRunCreationError . show) $ void checkResult

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
webhookMiddleware :: GlobalSettings -> ViraRuntimeState -> WebSettings -> Middleware
webhookMiddleware globalSettings viraRuntimeState webSettings app req sendResponse =
  case pathInfo req of
    ("webhook" : "github" : rest) -> do
      -- Route to webhook sub-app with path stripped
      let req' = req {pathInfo = rest}
      webhookApp req' sendResponse
    _ -> app req sendResponse
  where
    key = maybe mempty encodeUtf8 webSettings.githubWebhookSecret
    githubKey = GitHubKey $ pure key
    webhookApp =
      genericServeTWithContext
        id
        (handlers globalSettings viraRuntimeState webSettings)
        (githubKey :. EmptyContext)
