{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.Supervisor.Task (
  -- * Main supervisor operations
  startTask,
  killTask,

  -- * Log sink management
  createTaskLogSink,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), log, withLogContext)
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar_, readMVar)
import Effectful.Environment (Environment)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import LogSink (Sink (..))
import LogSink.Broadcast (Broadcast (..), bcClose, broadcastSink, newBroadcast)
import LogSink.Contrib.NixNoise (noiseGroupingSink)
import LogSink.File (fileSink)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Vira.CI.Log (ViraLog (..), encodeViraLog)

import Vira.Supervisor.Type (Task (..), TaskId, TaskInfo (..), TaskState (..), TaskSupervisor (..), Terminated (Terminated))
import Prelude hiding (readMVar)

{- | Start a 'Task' in the 'TaskSupervisor'

  The orchestrator is a function that runs the actual task logic, and is provided
  with the necessary 'Effectful.Eff' capabilities. It uses the @Error err@ effect to report failures.

  The @onFinish@ handler is called when the task completes, whether successfully or due to an exception.
  It receives an @Either err ExitCode@ result from the orchestrator (derived from the 'Effectful.Error.Static.Error' effect).

  The working directory for the task is created if it does not exist, and an empty log file is initialized.
  The task's output is logged to this file, and a 'System.Tail.Tail' handle is created for potential log streaming.

  Note: This function assumes that the caller has ensured that no other task with the same 'TaskId' is running.
-}
startTask ::
  forall es err.
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , Environment :> es
  , HasCallStack
  , Show err
  , ER.Reader LogContext :> es
  ) =>
  TaskSupervisor ->
  TaskId ->
  Severity ->
  FilePath ->
  -- | Broadcast for SSE streaming
  Broadcast Text ->
  -- | Logging callback for supervisor-level messages (before/after pipeline)
  (Severity -> Text -> Eff es ()) ->
  -- | Orchestrator (closes over its own dependencies)
  ( forall es1.
    ( Concurrent :> es1
    , Process :> es1
    , Log (RichMessage IO) :> es1
    , IOE :> es1
    , FileSystem :> es1
    , ER.Reader LogContext :> es1
    , Error err :> es1
    , Environment :> es1
    ) =>
    Eff es1 ()
  ) ->
  -- | Handler to call after the task finishes
  ( -- Exit code
    Either err ExitCode ->
    Eff es ()
  ) ->
  Eff es ()
startTask supervisor taskId minSeverity workDir broadcast logFn orchestrator onFinish = do
  logSupervisorState supervisor
  modifyMVar_ (tasks supervisor) $ \tasks -> do
    if Map.member taskId tasks
      then do
        log Error "Task already exists"
        die "Task already exists"
      else do
        -- Log starting message (filter out workspace-level context like repo/branch/job
        -- since it's already encoded in the file path and would be redundant)
        when (Info >= minSeverity) $
          logFn Info ("Starting task " <> show taskId)

        asyncHandle <- async $ do
          result <- runErrorNoCallStack orchestrator
          -- Convert () to ExitSuccess
          let exitResult = result $> ExitSuccess
          -- Log the errors if any
          whenLeft_ exitResult $ \err -> do
            when (Error >= minSeverity) $
              logFn Error (show err)

          -- Call the original handler
          onFinish exitResult
        let info = TaskInfo {..}
        let task = Task {..}
        pure $ Map.insert taskId task tasks

{- | Create task-specific log sink (file + broadcast)

Returns the sink, broadcast, and a cleanup action that must be called when the task completes.
This is NOT a bracket because the sink must outlive the synchronous portion of startTask.
-}
createTaskLogSink ::
  (IOE :> es, FileSystem :> es) =>
  -- | Working directory (will be created if missing)
  FilePath ->
  -- | Log file path within working directory
  FilePath ->
  Eff es (Sink Text, Broadcast Text, IO ())
createTaskLogSink workDir logFileName = do
  createDirectoryIfMissing True workDir
  let logFilePath = workDir </> logFileName
  fSink <- liftIO $ fileSink logFilePath
  broadcast <- liftIO $ newBroadcast 1000
  -- Combined sink: file (hPutStrLn adds \n) + broadcast (needs manual \n)
  let baseSink = fSink <> contramap (<> "\n") (broadcastSink broadcast)
  -- Wrap with noise grouping to collapse Nix input noise
  -- The encoder wraps NixNoise JSON in ViraLog Debug format
  let noiseEncoder noiseJson = encodeViraLog $ ViraLog Debug noiseJson (LogContext [])
  logSink <- liftIO $ noiseGroupingSink noiseEncoder baseSink
  let cleanup = sinkClose logSink >> sinkClose fSink >> bcClose broadcast
  pure (logSink, broadcast, cleanup)

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

logSupervisorState :: (HasCallStack, Concurrent :> es, Log (RichMessage IO) :> es, IOE :> es, ER.Reader LogContext :> es) => TaskSupervisor -> Eff es ()
logSupervisorState supervisor = do
  tasks <- readMVar (tasks supervisor)
  withFrozenCallStack $ log Debug $ "Current tasks: " <> show (Map.keys tasks)
  forM_ (Map.toList tasks) $ \(_, task) -> do
    st <- taskState task
    withFrozenCallStack $ log Debug $ "Task state: " <> show st
