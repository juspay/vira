{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.Supervisor.Task (
  -- * Main supervisor operations
  startTask,
  killTask,

  -- * Utilities for individual task orchestrators
  logToWorkspaceOutput,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar_, readMVar)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Tail qualified as Tail
import Vira.Lib.Logging (LogContext, log, withLogContext)
import Vira.Supervisor.Type (Task (..), TaskId, TaskInfo (..), TaskState (..), TaskSupervisor (..), Terminated (Terminated))
import Prelude hiding (readMVar)

{- | Start a task in the supervisor

  The orchestrator is a function that runs the actual task logic (using `runProcesses`), and is provided
  with the necessary Effectful capabilities. It should return an `Either Terminated ExitCode`
  indicating the result of the task.

  The `onFinish` handler is called when the task completes, whether successfully or due to an exception.
  It receives the same `Either Terminated ExitCode` result from the orchestrator.

  The working directory for the task is created if it does not exist, and an empty log file is initialized.
  The task's output is logged to this file, and a tail handle is created for potential log streaming.

  Note: This function assumes that the caller has ensured that no other task with the same `TaskId` is running.
-}
startTask ::
  forall es err.
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , HasCallStack
  , Show err
  , ER.Reader LogContext :> es
  ) =>
  TaskSupervisor ->
  TaskId ->
  FilePath ->
  ( forall es1.
    ( Concurrent :> es1
    , Process :> es1
    , Log (RichMessage IO) :> es1
    , IOE :> es1
    , FileSystem :> es1
    , ER.Reader LogContext :> es1
    ) =>
    TaskId ->
    (forall es2. (IOE :> es2) => Text -> Eff es2 ()) ->
    Eff es1 (Either err ExitCode)
  ) ->
  -- Handler to call after the task finishes
  ( -- Exit code
    Either err ExitCode ->
    Eff es ()
  ) ->
  Eff es ()
startTask supervisor taskId workDir orchestrator onFinish = withLogContext [("task", show taskId)] $ do
  logSupervisorState supervisor
  let msg = "Starting Vira pipeline in " <> toText workDir
  log Info msg
  modifyMVar_ (tasks supervisor) $ \tasks -> do
    if Map.member taskId tasks
      then do
        log Error "Task already exists"
        die "Task already exists"
      else do
        createDirectoryIfMissing True workDir
        appendFileText (outputLogFile workDir) "" -- Create empty log file before tail starts
        tailHandle <- liftIO $ Tail.tailFile 1000 (outputLogFile workDir)
        logToWorkspaceOutput taskId workDir msg
        asyncHandle <- async $ do
          result <- orchestrator taskId (logToWorkspaceOutput taskId workDir)
          -- Log the errors if any
          whenLeft_ result $ \err -> do
            logToWorkspaceOutput taskId workDir $ "❌ ERROR: " <> show err
          -- Stop the tail when task finishes for any reason
          liftIO $ Tail.tailStop tailHandle
          -- Then call the original handler
          onFinish result
        let info = TaskInfo {..}
        let task = Task {..}
        pure $ Map.insert taskId task tasks

-- | Kill an active task
killTask :: (Concurrent :> es, Log (RichMessage IO) :> es, IOE :> es, ER.Reader LogContext :> es) => TaskSupervisor -> TaskId -> Eff es ()
killTask supervisor taskId = withLogContext [("task", show taskId)] $ do
  modifyMVar_ (tasks supervisor) $ \tasks -> do
    case Map.lookup taskId tasks of
      Nothing -> do
        log Warning "Attempted to kill non-existent task"
        pure tasks -- Don't modify the map if task doesn't exist
      Just task -> do
        log Info "Killing task"
        cancelWith task.asyncHandle Terminated
        pure $ Map.delete taskId tasks

taskState :: (Concurrent :> es) => Task -> Eff es TaskState
taskState Task {..} = do
  status <- poll asyncHandle
  case status of
    Nothing -> pure Running
    Just (Right _) -> pure $ Finished ExitSuccess
    Just (Left _) -> pure Killed

-- Send all output to a file under working directory.
outputLogFile :: FilePath -> FilePath
outputLogFile base = base </> "output.log"

logSupervisorState :: (HasCallStack, Concurrent :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es) => TaskSupervisor -> Eff es ()
logSupervisorState supervisor = do
  tasks <- readMVar (tasks supervisor)
  withFrozenCallStack $ log Debug $ "Current tasks: " <> show (Map.keys tasks)
  forM_ (Map.toList tasks) $ \(_, task) -> do
    st <- taskState task
    withFrozenCallStack $ log Debug $ "Task state: " <> show st

-- TODO: In lieu of https://github.com/juspay/vira/issues/6
logToWorkspaceOutput :: (IOE :> es) => TaskId -> FilePath -> Text -> Eff es ()
logToWorkspaceOutput taskId workDir (msg :: Text) = do
  let s = "🥕 [vira:job:" <> show taskId <> "] " <> msg <> "\n"
  appendFileText (outputLogFile workDir) s
