{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub check run lifecycle management

Creates and updates GitHub check runs for PR jobs.
The 'prCheckRunWatcher' is the unified entry point: spawned by the webhook handler
for every PR event, it waits for a job to appear (immediate for same-repo,
after approval for fork PRs) then creates and watches the check run.
-}
module Vira.GitHub.CheckRun (
  -- * Unified PR watcher
  prCheckRunWatcher,

  -- * Check run lifecycle (internal)
  jobStatusLoop,
) where

import Colog.Message (RichMessage)
import Control.Concurrent.STM (TChan)
import Data.Acid.Events (SomeUpdate)
import Data.Acid.Events qualified as Events
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), Severity (..), log)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Git (CommitID (..), RepoName)
import Effectful.Reader.Static qualified as ER
import Vira.Effect.GitHub
import Vira.Lib.GitHub
import Vira.State.Acid (AddNewJobA (..), JobUpdateStatusA (..))
import Vira.State.Core (ViraState)
import Vira.State.Type (Job (..), JobId, JobResult (..), JobStatus (..))
import Prelude hiding (Reader)

{- | Unified PR check run watcher

Spawned by the webhook handler for every PR event (open/reopen/synchronize).
Waits for a job matching this PR + commit, then creates a GitHub check run
and watches for status updates until the job finishes.

For same-repo PRs: the job is enqueued before this watcher starts, so the
matching 'AddNewJobA' event is found immediately.
For fork PRs: this watcher blocks until the core approval route enqueues the job.

The @installationId@ is captured in the async closure — never persisted in core state.
-}
prCheckRunWatcher ::
  ( GitHub :> es
  , ER.Reader LogContext :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  ) =>
  TChan (SomeUpdate ViraState) ->
  InstallationId ->
  Owner ->
  Repo ->
  RepoName ->
  Int ->
  CommitID ->
  Eff es ()
prCheckRunWatcher chan instId owner repo repoName prNum commitId = do
  -- Wait for a job matching this PR + commit
  log Info $ "Watching for job on PR #" <> show prNum <> " commit " <> show commitId
  jobId <- liftIO $ waitForPRJob chan repoName prNum commitId
  log Info $ "Found job " <> show jobId <> " for PR #" <> show prNum

  -- Create check run and watch status
  checkRun <-
    queryGitHub @CheckRun instId $
      createCheckRunE owner repo $
        NewCheckRun
          { name = "Vira CI"
          , headSha = unCommitID commitId
          , status = Just Queued
          }
  log Info $ "Created check run for PR #" <> show prNum <> " commit " <> show commitId
  jobStatusLoop chan instId owner repo checkRun.checkRunId jobId

-- | Wait for an 'AddNewJobA' event matching the given PR and commit
waitForPRJob :: TChan (SomeUpdate ViraState) -> RepoName -> Int -> CommitID -> IO JobId
waitForPRJob chan repoName prNum commitId = do
  updates <- Events.awaitBatched chan matchesPRJob 500_000
  -- The last matching event has the job we want
  let extractJobId u = case Events.matchUpdate @AddNewJobA u of
        Just (AddNewJobA r _ c (Just n) _ _, job) | r == repoName && n == prNum && c == commitId -> Just job.jobId
        _ -> Nothing
  case mapMaybe extractJobId (toList updates) of
    (jid : _) -> pure jid
    [] -> waitForPRJob chan repoName prNum commitId -- shouldn't happen, but retry
  where
    matchesPRJob update =
      case Events.matchUpdate @AddNewJobA update of
        Just (AddNewJobA r _ c (Just n) _ _, _) -> r == repoName && n == prNum && c == commitId
        _ -> False

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
