{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.Supervisor.Task (
  -- * Main supervisor operations
  startTask,
  killTask,

  -- * Utilities for individual task orchestrators
  AppTaskStack,
  runProcesses,
  logToWorkspaceOutput,
) where

import Colog (Message, Severity (..))
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar_, readMVar)
import Effectful.Exception (catch, finally, mask)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.FileSystem.IO (hClose, openFile)
import Effectful.Process (CreateProcess (cmdspec, create_group), Pid, Process, createProcess, getPid, interruptProcessGroupOf, waitForProcess)
import Effectful.Reader.Dynamic (Reader)
import Effectful.Reader.Dynamic qualified as Reader
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Tail qualified as Tail
import Vira.Lib.Logging (log, tagCurrentThread)
import Vira.Lib.Process qualified as Process
import Vira.Supervisor.Type (Task (..), TaskException (KilledByUser), TaskId, TaskInfo (..), TaskState (..), TaskSupervisor (..))
import Prelude hiding (Reader, readMVar, runReader)

type AppTaskStack es =
  ( Concurrent :> es
  , Process :> es
  , Log Message :> es
  , IOE :> es
  , FileSystem :> es
  , Reader TaskInfo :> es
  )

{- | Start a task in the supervisor

  The orchestrator is a function that runs the actual task logic (using `runProcesses`), and is provided
  with the necessary Effectful capabilities. It should return an `Either TaskException ExitCode`
  indicating the result of the task.

  The `onFinish` handler is called when the task completes, whether successfully or due to an exception.
  It receives the same `Either TaskException ExitCode` result from the orchestrator.

  The working directory for the task is created if it does not exist, and an empty log file is initialized.
  The task's output is logged to this file, and a tail handle is created for potential log streaming.

  Note: This function assumes that the caller has ensured that no other task with the same `TaskId` is running.
-}
startTask ::
  forall es err.
  ( Concurrent :> es
  , Process :> es
  , Log Message :> es
  , IOE :> es
  , FileSystem :> es
  , HasCallStack
  , Show err
  ) =>
  TaskSupervisor ->
  TaskId ->
  FilePath ->
  ( forall es1.
    ( AppTaskStack es1
    ) =>
    Eff es1 (Either err ExitCode)
  ) ->
  -- Handler to call after the task finishes
  ( -- Exit code
    Either err ExitCode ->
    Eff es ()
  ) ->
  Eff es ()
startTask supervisor taskId workDir orchestrator onFinish = do
  logSupervisorState supervisor
  let msg = "Starting Vira pipeline in " <> toText workDir
  log Info msg
  modifyMVar_ (tasks supervisor) $ \tasks -> do
    if Map.member taskId tasks
      then do
        log Error $ "Task " <> show taskId <> " already exists"
        die $ "Task " <> show taskId <> " already exists"
      else do
        createDirectoryIfMissing True workDir
        appendFileText (outputLogFile workDir) "" -- Create empty log file before tail starts
        tailHandle <- liftIO $ Tail.tailFile 1000 (outputLogFile workDir)
        let info = TaskInfo {..}
        Reader.runReader info $ logToWorkspaceOutput msg
        asyncHandle <- async $ do
          result <- Reader.runReader info orchestrator
          -- Log the errors if any
          whenLeft_ result $ \err -> do
            Reader.runReader info $ logToWorkspaceOutput $ "❌ ERROR: " <> show err
          -- Stop the tail when task finishes for any reason
          liftIO $ Tail.tailStop tailHandle
          -- Then call the original handler
          onFinish result
        let task = Task {..}
        pure $ Map.insert taskId task tasks

-- | Run a sequence of processes, stopping on first failure
runProcesses ::
  forall es.
  ( AppTaskStack es
  ) =>
  -- List of processes to run in sequence
  NonEmpty CreateProcess ->
  Eff es (Either TaskException ExitCode)
runProcesses procs = do
  taskId <- Reader.asks taskId
  tagCurrentThread $ "🪜 ;task=" <> show taskId
  runProcs $ toList procs
  where
    -- Run each process one after another; exiting immediately if any fails
    runProcs :: [CreateProcess] -> Eff es (Either TaskException ExitCode)
    runProcs [] = do
      log Info "All procs for task finished successfully"
      pure $ Right ExitSuccess
    runProcs (process : rest) =
      runProc process >>= \case
        (pid, Right ExitSuccess) -> do
          log Debug $ "A proc for task finished successfully (pid=" <> show pid <> ")."
          runProcs rest
        (pid, Right exitCode) -> do
          log Warning $ "A proc for task failed with exitCode " <> show exitCode <> " (pid=" <> show pid <> ")."
          pure $ Right exitCode
        (pid, Left e) -> do
          log Warning $ "A proc for task (pid=" <> show pid <> ") was interrupted:  " <> show e
          pure $ Left e

    runProc :: CreateProcess -> Eff es (Maybe Pid, Either TaskException ExitCode)
    runProc process = do
      taskId <- Reader.asks taskId
      workDir <- Reader.asks workDir
      tagCurrentThread $ "🪜 ;task=" <> show taskId
      log Debug $ "Starting task: " <> show (cmdspec process)
      logToWorkspaceOutput $ "Starting task: " <> show (cmdspec process)
      withFileHandle (outputLogFile workDir) AppendMode $ \outputHandle -> do
        let processSettings =
              Process.alwaysUnderPath workDir
                >>> Process.redirectOutputTo outputHandle
                >>> (\cp -> cp {create_group = True}) -- For `interruptProcessGroupOf`, when the process is `KilledByUser`
        (_, _, _, ph) <- createProcess $ process & processSettings
        pid <- getPid ph
        tagCurrentThread $ "🪜 ;task=" <> show taskId <> ";pid=" <> maybe "?" show pid
        log Info $ "Task spawned : " <> show (cmdspec process)

        result <-
          -- `mask` cleanup from asynchronous interruptions
          mask $ \restore ->
            restore (Right <$> waitForProcess ph)
              `catch` \case
                KilledByUser -> do
                  log Info "Terminating process"
                  interruptProcessGroupOf ph `catch` \(e :: SomeException) ->
                    -- Analogous to `Control-C`'ing the process in an interactive shell
                    log Error $ "Failed to terminate process: " <> show e
                  _ <- waitForProcess ph -- Reap to prevent zombies
                  pure $ Left KilledByUser
        log Debug $ "Task finished: " <> show (cmdspec process)
        logToWorkspaceOutput $
          "A task (pid=" <> show pid <> ") finished with " <> either (("exception: " <>) . toText . displayException) (("exitcode: " <>) . show) result
        log Debug "Workspace log done"
        pure (pid, result)

-- | Kill an active task
killTask :: (Concurrent :> es, Log Message :> es, IOE :> es) => TaskSupervisor -> TaskId -> Eff es ()
killTask supervisor taskId = do
  modifyMVar_ (tasks supervisor) $ \tasks -> do
    case Map.lookup taskId tasks of
      Nothing -> do
        log Warning $ "Attempted to kill non-existent task " <> show taskId
        pure tasks -- Don't modify the map if task doesn't exist
      Just task -> do
        log Info $ "Killing task " <> show taskId
        cancelWith task.asyncHandle KilledByUser
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

-- | Helper function that provides withFile-like behavior for Effectful
withFileHandle :: (FileSystem :> es, IOE :> es) => FilePath -> IOMode -> (Handle -> Eff es a) -> Eff es a
withFileHandle path mode action = do
  handle <- openFile path mode
  finally (action handle) (hClose handle)

logSupervisorState :: (HasCallStack, Concurrent :> es, Log Message :> es) => TaskSupervisor -> Eff es ()
logSupervisorState supervisor = do
  tasks <- readMVar (tasks supervisor)
  withFrozenCallStack $ log Debug $ "Current tasks: " <> show (Map.keys tasks)
  forM_ (Map.toList tasks) $ \(taskId, task) -> do
    st <- taskState task
    withFrozenCallStack $ log Debug $ "Task " <> show taskId <> " state: " <> show st

-- TODO: In lieu of https://github.com/juspay/vira/issues/6
logToWorkspaceOutput :: (IOE :> es, Reader TaskInfo :> es) => Text -> Eff es ()
logToWorkspaceOutput (msg :: Text) = do
  taskId <- Reader.asks taskId
  workDir <- Reader.asks workDir
  let s = "🥕 [vira:job:" <> show taskId <> "] " <> msg <> "\n"
  appendFileText (outputLogFile workDir) s
