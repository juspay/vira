{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub check run lifecycle management

Creates and updates GitHub check runs for PR jobs.
Used by the webhook handler (same-repo PRs) and approval handler (fork PRs).
-}
module Vira.GitHub.CheckRun (
  -- * Check run lifecycle
  createCheckRunAndWatch,
  jobStatusLoop,

  -- * Approval
  ApprovalError (..),
  approvalHandler,
  approvalUrl,

  -- * Utilities
  splitRepoName,
) where

import Colog.Message (RichMessage)
import Control.Concurrent.STM (TChan)
import Data.Acid.Events (SomeUpdate)
import Data.Acid.Events qualified as Events
import Data.Text qualified as T
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), Severity (..), log)
import Effectful.Concurrent.Async (Concurrent, async)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Git (BranchName (..), CommitID (..), RepoName (..))
import Effectful.Reader.Dynamic (Reader)
import Effectful.Reader.Static qualified as ER
import Vira.App (AppStack)
import Vira.App.AcidState qualified as App
import Vira.App.Type (ViraRuntimeState)
import Vira.CI.Client qualified as Client
import Vira.Effect.GitHub
import Vira.Lib.GitHub
import Vira.State.Acid (JobUpdateStatusA (..))
import Vira.State.Acid qualified as St
import Vira.State.Core (ViraState)
import Vira.State.Type (Job (..), JobId, JobResult (..), JobStatus (..), PRCommit (..), PullRequest (..))
import Prelude hiding (Reader)

{- | Create a GitHub check run for a PR job and watch for status updates

Self-contained: interprets GitHub and Error effects internally.
Used by the approval handler for fork PR jobs.
-}
createCheckRunAndWatch ::
  ( Reader ViraRuntimeState :> es
  , Concurrent :> es
  , ER.Reader LogContext :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  ) =>
  AppAuth ->
  InstallationId ->
  Owner ->
  Repo ->
  CommitID ->
  JobId ->
  Eff es ()
createCheckRunAndWatch appAuth instId owner repo sha jobId = do
  chan <- App.subscribe
  void $ async $ do
    result <- runErrorNoCallStack @GitHubError $ runGitHubAsApp appAuth $ do
      checkRun <-
        queryGitHub @CheckRun instId $
          createCheckRunE owner repo $
            NewCheckRun
              { name = "Vira CI"
              , headSha = unCommitID sha
              , status = Just Queued
              }
      log Info $ "Created check run for commit " <> show sha
      jobStatusLoop chan instId owner repo checkRun.checkRunId jobId
    case result of
      Left err -> log Warning $ "GitHub check run error: " <> show err
      Right () -> pass

-- | Watch event bus for job status changes, updating the GitHub check run
jobStatusLoop ::
  ( GitHub :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  TChan (SomeUpdate ViraState) ->
  InstallationId ->
  Owner ->
  Repo ->
  CheckRunId ->
  JobId ->
  Eff es ()
jobStatusLoop chan instId owner repo checkRunId jobId = do
  updates <- liftIO $ Events.awaitBatched chan (matchesJob jobId) 500_000
  let latestStatus = lastStatus updates
  result <-
    runErrorNoCallStack @GitHubError $
      queryGitHub_ instId $
        updateCheckRunE owner repo checkRunId $
          fromJobStatus latestStatus
  case result of
    Left err -> log Warning $ "Failed to update check run " <> show checkRunId <> ": " <> show err
    Right () -> pass
  unless (isTerminal latestStatus) $ jobStatusLoop chan instId owner repo checkRunId jobId
  where
    matchesJob targetJobId update =
      case Events.matchUpdate @JobUpdateStatusA update of
        Just (JobUpdateStatusA jid _, _) -> jid == targetJobId
        Nothing -> False

    lastStatus updates =
      let extractStatus u = case Events.matchUpdate @JobUpdateStatusA u of
            Just (JobUpdateStatusA _ s, _) -> Just s
            Nothing -> Nothing
       in fromMaybe JobPending $ viaNonEmpty last $ mapMaybe extractStatus (toList updates)

    isTerminal = \case
      JobFinished {} -> True
      JobStale -> True
      _ -> False

-- | Convert a 'JobStatus' to a GitHub check run update
fromJobStatus :: JobStatus -> UpdateCheckRun
fromJobStatus = \case
  JobRunning ->
    UpdateCheckRun {status = InProgress, conclusion = Nothing}
  JobFinished jobResult _ -> do
    let conclusion = case jobResult of
          JobSuccess -> Success
          JobFailure -> Failure
          JobKilled -> Cancelled
    UpdateCheckRun {status = Completed, conclusion = Just conclusion}
  JobStale ->
    UpdateCheckRun {status = Completed, conclusion = Just Cancelled}
  JobPending -> UpdateCheckRun {status = Queued, conclusion = Nothing}

-- | Split a 'RepoName' like @\"owner\/repo\"@ into 'Owner' and 'Repo'
splitRepoName :: RepoName -> (Owner, Repo)
splitRepoName (RepoName name) =
  case T.breakOn "/" name of
    (owner, rest) -> (Owner owner, Repo $ T.drop 1 rest)

-- * Approval

-- | Errors that can occur during approval
data ApprovalError
  = ApprovalNotFound
  | CommitNotFound
  | AlreadyApproved

{- | Handle fork PR approval: update state and enqueue job

Returns the PR and created job ID on success.
-}
approvalHandler ::
  RepoName ->
  Int ->
  CommitID ->
  Eff AppStack (Either ApprovalError (PullRequest, JobId))
approvalHandler repoName prNum sha = do
  mPR <- App.query (St.GetPullRequestA repoName prNum)
  mCommit <- App.query $ St.GetPRCommitA repoName prNum sha
  case (mPR, mCommit) of
    (Nothing, _) -> pure $ Left ApprovalNotFound
    (_, Nothing) -> pure $ Left CommitNotFound
    (Just pr, Just pc)
      | pc.approved -> pure $ Left AlreadyApproved
      | otherwise -> do
          void $ App.update $ St.ApprovePRCommitA repoName prNum sha
          let branchRef = BranchName $ "refs/pull/" <> show prNum <> "/head"
          job <- Client.enqueueJob pr.repoName branchRef pc.sha (Just prNum)
          pure $ Right (pr, job.jobId)

-- | Build the approval URL for the GitHub middleware route
approvalUrl :: RepoName -> Int -> CommitID -> Text
approvalUrl (RepoName repo) prNum (CommitID sha) =
  "/github/r/" <> repo <> "/pull/" <> show prNum <> "/approve/" <> sha
