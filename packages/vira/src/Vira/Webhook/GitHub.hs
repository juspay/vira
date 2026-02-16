{- | GitHub App webhook handler as WAI sub-app

This module provides a standalone WAI application for handling GitHub webhook events.
It is decoupled from the main Vira web server and can be mounted as middleware.
-}
module Vira.Webhook.GitHub (
  webhookMiddleware,
  notConfiguredMiddleware,
) where

import Colog (Severity (..))
import Crypto.PubKey.RSA (PrivateKey)
import Effectful (Eff)
import Effectful qualified as Eff
import Effectful.Colog.Simple (log, tagCurrentThread)
import Effectful.Error.Static as Eff (Error, runErrorNoCallStack)
import Effectful.Reader.Dynamic as Eff (Reader, runReader)
import GitHub.Data.Webhooks.Events (
  PullRequestEvent (..),
  PushEvent,
 )
import GitHub.Data.Webhooks.Payload (HookPullRequest (..), HookRepository (..), HookSimpleUser (..), HookUser (..), PullRequestTarget (..))
import Network.HTTP.Types (status503)
import Network.Wai (Middleware, pathInfo, responseLBS)
import Servant
import Servant.GitHub.Webhook (GitHubEvent, GitHubKey (..))
import Servant.Server.Generic (AsServer, genericServeTWithContext)
import Vira.App (AppStack, GlobalSettings, ViraRuntimeState, runApp)
import Vira.App.CLI (GHAppAuthSettings (..), WebSettings (..))
import Vira.Effect.GitHub
import Vira.Lib.GitHub

-- | API type for GitHub webhook events
data Routes mode = Routes
  { _pr :: mode :- GitHubEvent PullRequestEvent :> Post '[JSON] NoContent
  , _push :: mode :- GitHubEvent PushEvent :> Post '[JSON] NoContent
  }
  deriving stock (Generic)

-- | Servant handlers for GitHub webhook events
handlers ::
  GlobalSettings ->
  ViraRuntimeState ->
  WebSettings ->
  GHAppAuthSettings ->
  PrivateKey ->
  InstallationAccessTokens ->
  Routes AsServer
handlers globalSettings viraRuntimeState webSettings appSettings privateKey iats =
  Routes
    { _pr = runWebhookInServant globalSettings viraRuntimeState webSettings appSettings privateKey iats . prHandler
    , _push = runWebhookInServant globalSettings viraRuntimeState webSettings appSettings privateKey iats . pushHandler
    }

-- | Handle pull request events and create a GitHub Check run
prHandler :: PullRequestEvent -> Eff WebhookStack NoContent
prHandler event = do
  log Info $ "Received PR: " <> show event
  createCheckRun event
  pure NoContent
  where
    createCheckRun ::
      (GitHub Eff.:> es) =>
      PullRequestEvent ->
      Eff es ()
    createCheckRun prEvent = do
      let repo = evPullReqRepo prEvent
          prTarget = whPullReqHead $ evPullReqPayload prEvent
          instId = InstallationId $ fromMaybe 0 $ evPullReqInstallationId prEvent

      queryGitHub_
        instId
        ( createCheckRunE
            (Owner $ getLogin (whRepoOwner repo))
            (Repo $ whRepoName repo)
            (mkNewCheckRun prTarget)
        )
      where
        getLogin = either (fromMaybe "" . whSimplUserLogin) whUserLogin
        mkNewCheckRun prTarget =
          NewCheckRun
            { name = "Vira CI"
            , headSha = whPullReqTargetSha prTarget
            , status = Just InProgress
            }

pushHandler :: PushEvent -> Eff WebhookStack NoContent
pushHandler _ = do
  log Info "Received Push"
  pure NoContent

type WebhookStack = GitHub : Error GitHubError : Error ServerError : Eff.Reader WebSettings : AppStack

{- | Run webhook stack into Servant Handler.
Any GitHubError is logged and swallowed (webhook returns 200 regardless).
-}
runWebhookInServant ::
  GlobalSettings ->
  ViraRuntimeState ->
  WebSettings ->
  GHAppAuthSettings ->
  PrivateKey ->
  InstallationAccessTokens ->
  Eff WebhookStack NoContent ->
  Handler NoContent
runWebhookInServant globalSettings viraRuntimeState webSettings appSettings privateKey iats action =
  Handler
    . ExceptT
    . runApp globalSettings viraRuntimeState
    . Eff.runReader webSettings
    . runErrorNoCallStack @ServerError
    . logAndSwallowGitHubError
    . runGitHub privateKey (AppId appSettings.appId) iats
    $ do
      tagCurrentThread "🪝"
      action
  where
    logAndSwallowGitHubError ::
      Eff (Error GitHubError : Error ServerError : Eff.Reader WebSettings : AppStack) NoContent ->
      Eff (Error ServerError : Eff.Reader WebSettings : AppStack) NoContent
    logAndSwallowGitHubError m = do
      result <- runErrorNoCallStack @GitHubError m
      case result of
        Left err -> do
          log Warning $ "GitHub API error: " <> show err
          pure NoContent
        Right a -> pure a

{- | WAI middleware that mounts the GitHub webhook at @\/webhook\/github@
TODO: appropriate doc comment
-}
webhookMiddleware ::
  InstallationAccessTokens ->
  GlobalSettings ->
  ViraRuntimeState ->
  WebSettings ->
  GHAppAuthSettings ->
  PrivateKey ->
  Middleware
webhookMiddleware iats globalSettings viraRuntimeState webSettings appSettings privateKey = \app req sendResponse ->
  case pathInfo req of
    ("webhook" : "github" : rest) -> do
      let req' = req {pathInfo = rest}
      webhookApp req' sendResponse
    _ -> app req sendResponse
  where
    key = maybe mempty encodeUtf8 webSettings.ghWebhookSecret
    githubKey = GitHubKey $ pure key
    webhookApp =
      genericServeTWithContext
        id
        (handlers globalSettings viraRuntimeState webSettings appSettings privateKey iats)
        (githubKey :. EmptyContext)

{- | WAI middleware that returns 503 with an appropriate message when required Vira CLI args are not configured
TODO: appropriate function name and doc comment
On a second thought, this can be inlined in Server.hs because this module needn't have to be concerned about the server's problems
-}
notConfiguredMiddleware :: Middleware
notConfiguredMiddleware app req sendResponse =
  case pathInfo req of
    ("webhook" : "github" : _) ->
      sendResponse $
        responseLBS
          status503
          [("Content-Type", "text/plain")]
          "GitHub webhook not configured. Set --github-app-id and --github-app-private-key to enable."
    _ -> app req sendResponse
