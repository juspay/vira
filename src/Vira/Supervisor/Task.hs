{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.Supervisor.Task (
  startTask,
  killTask,
  getOrCreateFileTailer,
) where

import Control.Concurrent.MVar qualified
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar, modifyMVar_, readMVar)
import Effectful.Exception (catch, finally, mask)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.FileSystem.IO (hClose, openFile)
import Effectful.Process (CreateProcess (cmdspec, create_group), Pid, Process, createProcess, getPid, interruptProcessGroupOf, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Vira.App.Logging
import Vira.Lib.FileTailer qualified as FileTailer
import Vira.Lib.Process qualified as Process
import Vira.Supervisor.Type
import Prelude hiding (readMVar)

logSupervisorState :: (HasCallStack, Concurrent :> es, Log Message :> es) => TaskSupervisor -> Eff es ()
logSupervisorState supervisor = do
  tasks <- readMVar (tasks supervisor)
  withFrozenCallStack $ log Debug $ "Current tasks: " <> show (Map.keys tasks)
  forM_ (Map.toList tasks) $ \(taskId, task) -> do
    st <- taskState task
    withFrozenCallStack $ log Debug $ "Task " <> show taskId <> " state: " <> show st

-- TODO: In lieu of https://github.com/juspay/vira/issues/6
logToWorkspaceOutput :: (IOE :> es) => TaskId -> FilePath -> Text -> Eff es ()
logToWorkspaceOutput taskId base (msg :: Text) = do
  let s = "🥕 [vira:job:" <> show taskId <> "] " <> msg <> "\n"
  appendFileText (outputLogFile base) s

-- | Get existing or create new file tailer for a task
getOrCreateFileTailer :: Task -> FilePath -> IO FileTailer.FileTailer
getOrCreateFileTailer task logFile = do
  Control.Concurrent.MVar.modifyMVar (fileTailer task) $ \case
    Just tailer -> pure (Just tailer, tailer)
    Nothing -> do
      tailer <- FileTailer.startTailing logFile
      pure (Just tailer, tailer)

-- | Start a new a task, returning its working directory.
startTask ::
  ( Concurrent :> es
  , Process :> es
  , Log Message :> es
  , FileSystem :> es
  , IOE :> es
  , HasCallStack
  ) =>
  TaskSupervisor ->
  TaskId ->
  -- The working directory of the job (will be created)
  FilePath ->
  -- List of processes to run in sequence
  NonEmpty CreateProcess ->
  -- Handler to call after the task finishes
  ( -- Exit code
    Either TaskException ExitCode ->
    Eff es ()
  ) ->
  Eff es ()
startTask supervisor taskId pwd procs h = do
  logSupervisorState supervisor
  let msg = "Starting task group: " <> show (cmdspec <$> procs) <> " in " <> toText pwd
  log Info msg

  task <- modifyMVar (tasks supervisor) $ \tasks -> do
    if Map.member taskId tasks
      then do
        log Error $ "Task " <> show taskId <> " already exists"
        die $ "Task " <> show taskId <> " already exists; impossible!"
      else do
        createDirectoryIfMissing True pwd
        logToWorkspaceOutput taskId pwd msg
        asyncHandle <- async $ do
          hdl <- startTask' taskId pwd h procs
          logToWorkspaceOutput taskId pwd "CI finished"
          pure hdl
        fileTailer <- newMVar Nothing
        let task = Task {workDir = pwd, asyncHandle, fileTailer}
        pure (Map.insert taskId task tasks, task)

  -- Schedule cleanup independently, completely outside of any MVar scope
  void $ async $ do
    void $ wait task.asyncHandle -- Wait for task completion
    -- Clean up file tailer if any
    whenJustM (readMVar task.fileTailer) $ \tailer -> do
      log Info $ "Stopping file tailer for finished task " <> show taskId
      liftIO $ FileTailer.stopTailing tailer
    -- Remove task from supervisor immediately
    modifyMVar_ supervisor.tasks $ \currentTasks -> do
      log Debug $ "Removing finished task " <> show taskId <> " from supervisor"
      pure $ Map.delete taskId currentTasks

startTask' ::
  forall es.
  (Concurrent :> es, Process :> es, Log Message :> es, IOE :> es, FileSystem :> es) =>
  TaskId ->
  FilePath ->
  ( Either TaskException ExitCode ->
    Eff es ()
  ) ->
  -- List of processes to run in sequence
  NonEmpty CreateProcess ->
  Eff es (Either TaskException ExitCode)
startTask' taskId pwd h = runProcs . toList
  where
    -- Run each process one after another; exiting immediately if any fails
    runProcs :: [CreateProcess] -> Eff es (Either TaskException ExitCode)
    runProcs [] = do
      log Info $ "All procs for task " <> show taskId <> " finished successfully"
      h $ Right ExitSuccess
      pure $ Right ExitSuccess
    runProcs (proc : rest) =
      runProc proc >>= \case
        (pid, Right ExitSuccess) -> do
          log Debug $ "A proc for task " <> show taskId <> " (pid=" <> show pid <> ") successfully finished."
          runProcs rest
        (pid, Right exitCode) -> do
          log Warning $ "A proc for task " <> show taskId <> " (pid=" <> show pid <> ") failed with exitCode " <> show exitCode
          h $ Right exitCode
          pure $ Right exitCode
        (pid, Left e) -> do
          log Warning $ "A proc for task " <> show taskId <> " (pid=" <> show pid <> ") was interrupted:  " <> show e
          h $ Left e
          pure $ Left e

    runProc :: CreateProcess -> Eff es (Maybe Pid, Either TaskException ExitCode)
    runProc proc = do
      log Debug $ "Starting task: " <> show (cmdspec proc)
      logToWorkspaceOutput taskId pwd $ "Starting task: " <> show (cmdspec proc)
      withFileHandle (outputLogFile pwd) AppendMode $ \outputHandle -> do
        let processSettings =
              Process.alwaysUnderPath pwd
                >>> Process.redirectOutputTo outputHandle
                >>> (\cp -> cp {create_group = True}) -- For `interruptProcessGroupOf`, when the process is `KilledByUser`
        (_, _, _, ph) <- createProcess $ proc & processSettings
        pid <- getPid ph
        log Debug $ "Task spawned (pid=" <> show pid <> "): " <> show (cmdspec proc)

        result <-
          -- `mask` cleanup from asynchronous interruptions
          mask $ \restore ->
            restore (Right <$> waitForProcess ph)
              `catch` \case
                KilledByUser -> do
                  log Info $ "Terminating process (pid=" <> show pid <> ") for task " <> show taskId
                  interruptProcessGroupOf ph `catch` \(e :: SomeException) ->
                    -- Analogous to `Control-C`'ing the process in an interactive shell
                    log Error $ "Failed to terminate process " <> show pid <> " for task " <> show taskId <> ": " <> show e
                  _ <- waitForProcess ph -- Reap to prevent zombies
                  pure $ Left KilledByUser
        log Debug $ "Task finished (pid=" <> show pid <> "): " <> show (cmdspec proc)
        logToWorkspaceOutput taskId pwd $
          "A task (pid=" <> show pid <> ") finished with " <> either (("exception: " <>) . toText . displayException) (("exitcode: " <>) . show) result
        log Debug "Workspace log done"
        pure (pid, result)

-- | Kill an active task
killTask :: (Concurrent :> es, Log Message :> es, IOE :> es) => TaskSupervisor -> TaskId -> Eff es ()
killTask supervisor taskId = do
  taskMap <- readMVar (tasks supervisor)
  case Map.lookup taskId taskMap of
    Nothing -> do
      log Warning $ "Attempted to kill non-existent task " <> show taskId
    Just task -> do
      log Info $ "Killing task " <> show taskId
      -- Cancel the task - cleanup will happen automatically via the completion handler
      cancelWith task.asyncHandle KilledByUser

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
