{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.Supervisor where

import Control.Concurrent (forkIO)
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar, modifyMVar_, readMVar)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.Process (CreateProcess (cmdspec), Pid, Process, createProcess, getPid, waitForProcess)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Directory qualified
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Vira.App qualified as App
import Vira.App.Logging
import Vira.Lib.Logplex (LogPlex (writeHandle))
import Vira.Lib.Logplex qualified as Logplex
import Vira.Lib.Process qualified as Process
import Vira.Supervisor.Type
import Prelude hiding (readMVar)

newSupervisor :: (MonadIO m) => m TaskSupervisor
newSupervisor = do
  tasks <- newMVar mempty
  pwd <- liftIO getCurrentDirectory
  workDir <- liftIO $ makeAbsolute $ pwd </> "state" </> "workspace" -- keep it alongside acid-state db
  liftIO $ System.Directory.createDirectoryIfMissing True workDir
  pure $ TaskSupervisor tasks workDir

logSupervisorState :: (HasCallStack, Concurrent :> es, Log Message :> es) => TaskSupervisor -> Eff es ()
logSupervisorState supervisor = do
  tasks <- readMVar (tasks supervisor)
  withFrozenCallStack $ log Debug $ "Current tasks: " <> show (Map.keys tasks)
  forM_ (Map.toList tasks) $ \(taskId, task) -> do
    st <- taskState task
    withFrozenCallStack $ log Debug $ "Task " <> show taskId <> " state: " <> show st

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
    ExitCode ->
    Eff es ()
  ) ->
  Eff es ()
startTask supervisor taskId pwd procs h = do
  logSupervisorState supervisor
  let msg = "Starting task group: " <> show (cmdspec <$> procs) <> " in " <> toText pwd
  log Info msg
  modifyMVar (tasks supervisor) $ \tasks -> do
    if Map.member taskId tasks
      then do
        log Error $ "Task " <> show taskId <> " already exists"
        die $ "Task " <> show taskId <> " already exists"
      else do
        createDirectoryIfMissing True pwd
        logplex <- liftIO $ Logplex.newLogPlex $ pwd </> "output.log"
        _ <- liftIO $ forkIO $ Logplex.runLogPlex logplex

        logToWorkspaceOutput taskId logplex msg

        asyncHandle <- async $ startTask' taskId pwd logplex h procs

        let task = Task {workDir = pwd, asyncHandle, logplex}
        pure (Map.insert taskId task tasks, ())

-- TODO: In lieu of https://github.com/juspay/vira/issues/6
logToWorkspaceOutput :: (IOE :> es) => TaskId -> Logplex.LogPlex -> Text -> Eff es ()
logToWorkspaceOutput taskId logplex (msg :: Text) = do
  let s = "🥕 [vira:job:" <> show taskId <> "] " <> msg <> "\n"
  liftIO $ Logplex.write logplex s

startTask' ::
  forall es.
  (Process :> es, Log Message :> es, IOE :> es, FileSystem :> es) =>
  TaskId ->
  FilePath ->
  Logplex.LogPlex ->
  (ExitCode -> Eff es ()) ->
  -- List of processes to run in sequence
  NonEmpty CreateProcess ->
  Eff es ExitCode
startTask' taskId pwd logplex h procs = do
  runProcs $ toList procs
  where
    -- Run each process one after another; exiting immediately if any fails
    runProcs :: [CreateProcess] -> Eff es ExitCode
    runProcs [] = do
      log Info $ "All procs for task " <> show taskId <> " finished successfully"
      h ExitSuccess
      pure ExitSuccess
    runProcs (proc : rest) =
      runProc proc >>= \case
        (pid, ExitSuccess) -> do
          log Debug $ "A proc for task " <> show taskId <> " (pid=" <> show pid <> ") successfully finished."
          runProcs rest
        (pid, exitCode) -> do
          log Warning $ "A proc for task " <> show taskId <> " (pid=" <> show pid <> ") failed with exitCode " <> show exitCode
          h exitCode
          pure exitCode

    runProc :: CreateProcess -> Eff es (Maybe Pid, ExitCode)
    runProc proc = do
      log Debug $ "Starting task: " <> show (cmdspec proc)
      logToWorkspaceOutput taskId logplex $ "Starting task: " <> show (cmdspec proc)
      -- outputHandle <- openFile (outputLogFile pwd) AppendMode
      let processSettings =
            Process.alwaysUnderPath pwd
              >>> Process.redirectOutputTo logplex.writeHandle
      (_, _, _, ph) <- createProcess $ proc & processSettings
      pid <- getPid ph
      log Debug $ "Task spawned (pid=" <> show pid <> "): " <> show (cmdspec proc)
      exitCode <- waitForProcess ph
      log Debug $ "Task finished (pid=" <> show pid <> "): " <> show (cmdspec proc)
      -- hClose outputHandle
      logToWorkspaceOutput taskId logplex $ "A task (pid=" <> show pid <> ") finished with exit code " <> show exitCode
      pure (pid, exitCode)

-- | Kill a task
killTask :: TaskSupervisor -> TaskId -> Eff App.AppStack ()
killTask supervisor taskId = do
  log Info $ "Killing task " <> show taskId
  modifyMVar_ (tasks supervisor) $ \tasks -> do
    for_ (Map.lookup taskId tasks) $ \Task {..} ->
      cancel asyncHandle
    pure $ Map.delete taskId tasks

taskState :: (Concurrent :> es) => Task -> Eff es TaskState
taskState Task {..} = do
  status <- poll asyncHandle
  case status of
    Nothing -> pure Running
    Just (Right _) -> pure $ Finished ExitSuccess
    Just (Left _) -> pure Killed
