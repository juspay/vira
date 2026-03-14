{- | WAI middleware for GitHub routes

Intercepts requests to @/github/webhook/*@ and dispatches to webhook handlers.
-}
module Vira.GitHub.Middleware (
  githubMiddleware,
) where

import Network.Wai (Middleware, pathInfo)
import Servant (Context (..))
import Servant.GitHub.Webhook (GitHubKey (..))
import Servant.Server.Generic (genericServeTWithContext)
import Vira.App (GlobalSettings, ViraRuntimeState)
import Vira.Effect.GitHub (AppAuth)
import Vira.GitHub.Webhook qualified as Webhook

-- | WAI middleware that mounts GitHub webhook routes under @/github/webhook@
githubMiddleware ::
  GlobalSettings ->
  ViraRuntimeState ->
  AppAuth ->
  Text ->
  Middleware
githubMiddleware globalSettings viraRuntimeState appAuth webhookSecret app req sendResponse =
  case pathInfo req of
    ("github" : "webhook" : rest) -> do
      let req' = req {pathInfo = rest}
      webhookApp req' sendResponse
    _ -> app req sendResponse
  where
    key = encodeUtf8 webhookSecret
    githubKey = GitHubKey $ pure key
    webhookApp =
      genericServeTWithContext
        id
        (Webhook.handlers globalSettings viraRuntimeState appAuth)
        (githubKey :. EmptyContext)
