{-# HLINT ignore "Use next" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vira.Supervisor where

import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar, modifyMVar_, readMVar)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.FileSystem.IO (hClose, openFile)
import Effectful.Process (CreateProcess (cmdspec), Process, createProcess, waitForProcess)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Directory qualified
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Vira.App qualified as App
import Vira.App.Logging
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
  forM_ (Map.toList tasks) $ \(taskId, _) -> do
    taskState <- getTaskStatus supervisor taskId
    withFrozenCallStack $ log Debug $ "Task " <> show taskId <> " state: " <> show taskState

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
  log Info $ "Starting task group: " <> show (cmdspec <$> procs) <> " in " <> toText pwd
  modifyMVar (tasks supervisor) $ \tasks -> do
    if Map.member taskId tasks
      then do
        log Error $ "Task " <> show taskId <> " already exists"
        die $ "Task " <> show taskId <> " already exists"
      else do
        createDirectoryIfMissing True pwd
        asyncHandle <- Effectful.Concurrent.Async.async $ startTask' taskId pwd h procs
        let task = Task {workDir = pwd, asyncHandle}
        pure (Map.insert taskId task tasks, ())

startTask' ::
  forall es.
  (Process :> es, Log Message :> es, IOE :> es, FileSystem :> es) =>
  TaskId ->
  FilePath ->
  (ExitCode -> Eff es ()) ->
  -- List of processes to run in sequence
  NonEmpty CreateProcess ->
  Eff es ExitCode
startTask' taskId pwd h = runProcs . toList
  where
    -- Send all output to a file under working directory.
    -- Write vira level log entry to the output log
    outputLogFile = pwd </> "output.log"
    -- TODO: In lieu of https://github.com/juspay/vira/issues/6
    logToWorkspaceOutput (msg :: Text) = do
      let s = "🥕 [vira:job:" <> show taskId <> "] " <> msg <> "\n"
      appendFileText outputLogFile s

    -- Run each process one after another; exiting immediately if any fails
    runProcs :: [CreateProcess] -> Eff es ExitCode
    runProcs [] = do
      log Info $ "All procs for task " <> show taskId <> " finished successfully"
      h ExitSuccess
      pure ExitSuccess
    runProcs (proc : rest) =
      runProc proc >>= \case
        ExitSuccess -> runProcs rest
        exitCode -> do
          log Info $ "A proc for task " <> show taskId <> " failed with exitCode " <> show exitCode
          h exitCode
          pure exitCode

    runProc :: CreateProcess -> Eff es ExitCode
    runProc proc = do
      logToWorkspaceOutput $ "Task started: " <> show (cmdspec proc)
      outputHandle <- openFile outputLogFile AppendMode
      let processSettings =
            Process.alwaysUnderPath pwd
              >>> Process.redirectOutputTo outputHandle
      (_, _, _, ph) <- createProcess $ proc & processSettings
      exitCode <- waitForProcess ph
      logToWorkspaceOutput $ "A task finished with exit code " <> show exitCode
      hClose outputHandle
      pure exitCode

-- | Kill a task
killTask :: TaskSupervisor -> TaskId -> Eff App.AppStack ()
killTask supervisor taskId = do
  log Info $ "Killing task " <> show taskId
  modifyMVar_ (tasks supervisor) $ \tasks -> do
    for_ (Map.lookup taskId tasks) $ \Task {..} ->
      cancel asyncHandle
    pure $ Map.delete taskId tasks

-- | Get the status of a task
getTaskStatus :: (Concurrent :> es) => TaskSupervisor -> TaskId -> Eff es TaskState
getTaskStatus supervisor taskId = do
  tasks <- readMVar (tasks supervisor)
  case Map.lookup taskId tasks of
    Nothing -> pure Killed
    Just Task {..} -> do
      status <- poll asyncHandle
      case status of
        Nothing -> pure Running
        Just (Right output) -> pure $ Finished output
        Just (Left _) -> pure Killed
