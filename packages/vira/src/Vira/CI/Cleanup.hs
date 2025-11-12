{-# LANGUAGE OverloadedRecordDot #-}

{- | Job cleanup operations

This module provides functions for cleaning up old jobs from both acid-state
and the filesystem.
-}
module Vira.CI.Cleanup (
  deleteOldJob,
) where

import Colog.Message (RichMessage)
import Data.Time (UTCTime, diffUTCTime)
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, Severity (..), log)
import Effectful.FileSystem (FileSystem)
import Effectful.Reader.Dynamic (Reader)
import Effectful.Reader.Static qualified as ER
import System.Directory (doesDirectoryExist, removePathForcibly)
import Vira.App.AcidState qualified as App
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Lib.TimeExtra (formatDuration)
import Vira.State.Acid qualified as St
import Vira.State.Type (Job (..), JobStatus (..))
import Prelude hiding (Reader)

{- | Delete an old job from both acid-state and filesystem

Performs the following operations:
1. Deletes the job from acid-state
2. Removes the job's workspace directory
3. Logs the deletion with job details

Handles missing directories gracefully (logs warning but continues).
-}
deleteOldJob ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , FileSystem :> es
  ) =>
  UTCTime ->
  Job ->
  Eff es ()
deleteOldJob now job = case job.jobStatus of
  JobFinished _ endTime -> do
    let age = formatDuration $ diffUTCTime now endTime

    -- Delete from acid-state
    void $ App.update $ St.DeleteJobA job.jobId

    -- Delete workspace directory
    let workDir = job.jobWorkingDir
    exists <- liftIO $ doesDirectoryExist workDir
    if exists
      then do
        liftIO $ removePathForcibly workDir
        log Info $
          "🧹 Deleted job "
            <> show job.jobId
            <> " ("
            <> toText job.repo
            <> "/"
            <> toText job.branch
            <> " @ "
            <> show job.commit
            <> ") - "
            <> age
            <> " old"
      else
        log Warning $
          "🧹 Job workspace already missing: "
            <> show job.jobId
            <> " (dir: "
            <> toText workDir
            <> ")"
  _ -> log Error $ "Attempted to delete non-finished job: " <> show job.jobId
