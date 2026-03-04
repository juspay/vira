{-# LANGUAGE OverloadedRecordDot #-}

{- | WAI middleware for GitHub routes

Intercepts requests to @/github/*\@ and dispatches to appropriate handlers:
- @/github/webhook/*\@ -> Webhook handlers
- @/github/r/:repo/pull/:num/approve/:sha\@ -> Approval handler
-}
module Vira.GitHub.Middleware (
  githubMiddleware,
) where

import Effectful.Concurrent.Async (async)
import Effectful.Git (CommitID (..), RepoName (..))
import Network.HTTP.Types (status200, status400, status404)
import Network.Wai (Middleware, Response, pathInfo, responseLBS)
import Servant (Context (..))
import Servant.GitHub.Webhook (GitHubKey (..))
import Servant.Server.Generic (genericServeTWithContext)
import Vira.App (GlobalSettings, ViraRuntimeState, runApp)
import Vira.Effect.GitHub (AppAuth)
import Vira.GitHub.CheckRun (ApprovalError (..))
import Vira.GitHub.CheckRun qualified as CheckRun
import Vira.GitHub.Webhook qualified as Webhook
import Vira.Lib.GitHub (InstallationId (..))
import Vira.State.Type (PullRequest (..))

-- | WAI middleware that mounts GitHub routes under @/github@
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
    ("github" : "r" : owner : repo : "pull" : prNumStr : "approve" : shaStr : _) -> do
      handleApproval (owner <> "/" <> repo) prNumStr shaStr
    _ -> app req sendResponse
  where
    -- Webhook sub-app
    key = encodeUtf8 webhookSecret
    githubKey = GitHubKey $ pure key
    webhookApp =
      genericServeTWithContext
        Prelude.id
        (Webhook.handlers globalSettings viraRuntimeState appAuth)
        (githubKey :. EmptyContext)

    -- Approval handler
    handleApproval repoParts prNumStr shaStr =
      case parseApprovalParams repoParts prNumStr shaStr of
        Nothing ->
          sendResponse $ responseLBS status400 [("Content-Type", "text/plain")] "Invalid approval URL"
        Just (repoName, prNum, sha) -> do
          result <- runApprovalHandler repoName prNum sha
          case result of
            Left err -> sendResponse $ approvalErrorResponse err
            Right () -> sendResponse approvalSuccessResponse

    runApprovalHandler :: RepoName -> Int -> CommitID -> IO (Either ApprovalError ())
    runApprovalHandler repoName prNum sha =
      runApp globalSettings viraRuntimeState $ do
        CheckRun.approvalHandler repoName prNum sha >>= \case
          Left err -> pure $ Left err
          Right (pr, jobId) -> do
            let instId = InstallationId pr.installationId
                (owner, repo) = CheckRun.splitRepoName pr.repoName
            void $ async $ CheckRun.createCheckRunAndWatch appAuth instId owner repo sha jobId
            pure $ Right ()

    parseApprovalParams :: Text -> Text -> Text -> Maybe (RepoName, Int, CommitID)
    parseApprovalParams repoParts prNumStr shaStr = do
      repoName <- Just $ RepoName repoParts
      prNum <- readMaybe (toString prNumStr)
      let sha = CommitID shaStr
      Just (repoName, prNum, sha)

    approvalErrorResponse :: ApprovalError -> Response
    approvalErrorResponse = \case
      ApprovalNotFound -> responseLBS status404 [("Content-Type", "text/plain")] "PR not found"
      CommitNotFound -> responseLBS status404 [("Content-Type", "text/plain")] "Commit not found"
      AlreadyApproved -> responseLBS status400 [("Content-Type", "text/plain")] "Already approved"

    approvalSuccessResponse :: Response
    approvalSuccessResponse =
      responseLBS
        status200
        [("Content-Type", "text/html"), ("HX-Refresh", "true")]
        "Approved"
