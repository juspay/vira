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
import Vira.State.Type (JobId)
import Prelude hiding (Reader)

{- | Delete an old job from both acid-state and filesystem

Performs the following operations:
1. Deletes the job from acid-state
2. Removes the job's workspace directory
3. Logs the deletion

Handles missing directories gracefully.
-}
deleteOldJob ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , FileSystem :> es
  ) =>
  UTCTime ->
  JobId ->
  UTCTime ->
  FilePath ->
  Eff es ()
deleteOldJob now jobId endTime workDir = do
  let age = formatDuration $ diffUTCTime now endTime

  -- Delete from acid-state
  void $ App.update $ St.DeleteJobA jobId

  -- Delete workspace directory
  exists <- liftIO $ doesDirectoryExist workDir
  if exists
    then do
      liftIO $ removePathForcibly workDir
      log Info $ "🧹 Deleted job " <> show jobId <> " (" <> age <> " old)"
    else
      log Warning $ "🧹 Job workspace already missing: " <> show jobId
