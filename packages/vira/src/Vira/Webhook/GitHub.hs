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
import GitHub.Data.Webhooks.Events (PullRequestEvent, PushEvent)
import Network.Wai (Middleware, pathInfo)
import Servant
import Servant.GitHub.Webhook (GitHubEvent, GitHubKey (..))
import Servant.Server.Generic (AsServer, genericServeTWithContext)
import Vira.App (GlobalSettings, ViraRuntimeState)
import Vira.App.CLI (WebSettings (..))
import Vira.Web.Stack (AppServantStack, runAppInServant)

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
    { _pr = runAppInServant globalSettings viraRuntimeState webSettings . prHandler
    , _push = runAppInServant globalSettings viraRuntimeState webSettings . pushHandler
    }

prHandler :: PullRequestEvent -> Eff AppServantStack NoContent
prHandler _ = do
  log Info "Received Pull Request"
  pure NoContent

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
