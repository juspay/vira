{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.Supervisor.Task (
  -- * Main supervisor operations
  startTask,
  killTask,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), log, withLogContext, withoutLogContext)
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar_, readMVar)
import Effectful.Environment (Environment)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Tail qualified as Tail
import Vira.CI.Log (ViraLog (..), encodeViraLog)
import Vira.Supervisor.Type (Task (..), TaskId, TaskInfo (..), TaskState (..), TaskSupervisor (..), Terminated (Terminated))
import Prelude hiding (readMVar)

{- | Start a 'Task' in the 'TaskSupervisor'

  The orchestrator is a function that runs the actual task logic, and is provided
  with the necessary 'Effectful' capabilities. It uses the @Error err@ effect to report failures.

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
    (forall es2. (Log (RichMessage IO) :> es2, ER.Reader LogContext :> es2, IOE :> es2) => Severity -> Text -> Eff es2 ()) ->
    Eff es1 ()
  ) ->
  -- Handler to call after the task finishes
  ( -- Exit code
    Either err ExitCode ->
    Eff es ()
  ) ->
  Eff es ()
startTask supervisor taskId minSeverity workDir orchestrator onFinish = do
  -- Capture workspace-level context keys at entry (e.g., repo, branch, job)
  -- These will be filtered out when writing to workspace-specific log
  LogContext workspaceCtx <- ER.ask
  let workspaceKeys = map fst workspaceCtx

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
        let
          logger :: (forall es2. (Log (RichMessage IO) :> es2, ER.Reader LogContext :> es2, IOE :> es2) => Severity -> Text -> Eff es2 ())
          logger msgSeverity msgText = do
            log msgSeverity msgText -- co-log to stdout
            when (msgSeverity >= minSeverity) $ -- only write to file if severity is high enough
              logToWorkspaceOutput workspaceKeys msgSeverity msgText
        logger Info msg
        asyncHandle <- async $ do
          result <- runErrorNoCallStack $ orchestrator logger
          -- Convert () to ExitSuccess
          let exitResult = result $> ExitSuccess
          -- Log the errors if any
          whenLeft_ exitResult $ \err -> do
            logger Error $ show err
          -- Stop the tail when task finishes for any reason
          liftIO $ Tail.tailStop tailHandle
          -- Then call the original handler
          onFinish exitResult
        let info = TaskInfo {..}
        let task = Task {..}
        pure $ Map.insert taskId task tasks
  where
    -- TODO: In lieu of https://github.com/juspay/vira/issues/6
    -- FIXME: Don't complect with Vira
    logToWorkspaceOutput :: forall es'. (Log (RichMessage IO) :> es', ER.Reader LogContext :> es', IOE :> es') => [Text] -> Severity -> Text -> Eff es' ()
    logToWorkspaceOutput excludeKeys severity msg = do
      -- Filter out workspace-level context (repo, branch, job) since it's redundant
      -- in workspace-scoped log file (already encoded in file path)
      withoutLogContext excludeKeys $ do
        ctx <- ER.ask
        let viraLog = ViraLog {level = severity, message = msg, context = ctx}
            jsonLog = encodeViraLog viraLog <> "\n"
        appendFileText (outputLogFile workDir) jsonLog

    -- Send all output to a file under working directory.
    -- FIXME: Don't complect with Vira
    outputLogFile :: FilePath -> FilePath
    outputLogFile base = base </> "output.log"

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
