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
import Effectful (Eff, IOE)
import Effectful qualified as Eff
import Effectful.Colog.Simple (log)
import GitHub (executeRequest, mkOwnerName, mkRepoName)
import GitHub.Data.Id qualified as GH
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
  withAppAuth,
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
  log Info $ "Received PR on github:" <> show event

  case webSettings.githubApp of
    Just ghAppSettings -> do
      res <- createCheckRun ghAppSettings event
      case res of
        Left err -> log Warning (show err) -- TODO: for which event?
        Right _ -> log Info "Check run created successfully" -- TODO: for which event?
    Nothing -> log Warning "GitHub App settings not configured in CLI, skipping check run creation" -- TODO: for which event? use logging with context?
  pure NoContent
  where
    getOwnerLogin :: Either HookSimpleUser HookUser -> Text
    getOwnerLogin = either (fromMaybe "" . whSimplUserLogin) whUserLogin

    mkNewCheckRun :: PullRequestTarget -> NewCheckRun
    mkNewCheckRun prTarget =
      NewCheckRun
        { newCheckRunName = "Vira CI"
        , newCheckRunHeadSha = whPullReqTargetSha prTarget
        , newCheckRunStatus = Just CheckRunInProgress
        , newCheckRunExternalId = Nothing
        , newCheckRunDetailsUrl = Nothing
        , newCheckRunStartedAt = Nothing
        , newCheckRunConclusion = Nothing
        , newCheckRunCompletedAt = Nothing
        , newCheckRunOutput = Nothing
        , newCheckRunActions = Nothing
        }
    createCheckRun :: (IOE Eff.:> es) => GitHubAppSettings -> PullRequestEvent -> Eff es (Either ChecksAPIError ())
    createCheckRun ghAppSettings prEvent = do
      let repo = evPullReqRepo prEvent
          repoName = whRepoName $ evPullReqRepo prEvent
          repoOwner = getOwnerLogin $ whRepoOwner repo
          prTarget = whPullReqHead $ evPullReqPayload prEvent

      case evPullReqInstallationId prEvent of
        Just installationId -> do
          result <- withAppAuth
            (GH.Id ghAppSettings.appId)
            (GH.Id installationId)
            ghAppSettings.privateKeyPath
            $ \auth -> do
              let
                req = createCheckRunR (mkOwnerName repoOwner) (mkRepoName repoName) (mkNewCheckRun prTarget)
              checkResult <- liftIO $ executeRequest auth req
              pure $ first (CheckRunCreationError . show) $ void checkResult
          case result of
            Left err -> pure $ Left $ CheckRunCreationError (show err)
            Right _ -> pure $ Right ()
        Nothing -> pure $ Left NoInstallationId

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
