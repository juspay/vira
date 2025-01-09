{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use next" #-}
module Vira.Supervisor where

import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar, modifyMVar_, readMVar)
import Effectful.FileSystem (FileSystem, createDirectory)
import Effectful.FileSystem.IO (openFile)
import Effectful.Process (CreateProcess (cwd, std_err, std_out), Process, StdStream (UseHandle), createProcess, shell, waitForProcess)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, makeAbsolute)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import Vira.App qualified as App
import Vira.App.Logging
import Vira.Supervisor.Type
import Prelude hiding (readMVar)

newSupervisor :: (MonadIO m) => m TaskSupervisor
newSupervisor = do
  tasks <- newMVar mempty
  pwd <- liftIO getCurrentDirectory
  workDir <- liftIO $ makeAbsolute $ pwd </> "state" </> "workspace" -- keep it alongside acid-state db
  liftIO $ createDirectoryIfMissing True workDir
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
  String ->
  -- Handler to call after the task finishes
  ( -- \| Working directory
    FilePath ->
    -- \| Exit code
    ExitCode ->
    Eff es ()
  ) ->
  Eff es FilePath
startTask supervisor taskId cmd h = do
  logSupervisorState supervisor
  log Info $ "Starting task: " <> toText cmd
  modifyMVar (tasks supervisor) $ \tasks -> do
    if Map.member taskId tasks
      then do
        log Error $ "Task " <> show taskId <> " already exists"
        die $ "Task " <> show taskId <> " already exists"
      else do
        let pwd = workDir supervisor </> show taskId
        createDirectory pwd
        task <- async $ do
          -- Send all output to a file under working directory.
          outputHandle <- openFile (pwd </> "output.log") WriteMode
          let process =
                (shell cmd)
                  { cwd = Just pwd
                  , std_out = UseHandle outputHandle
                  , std_err = UseHandle outputHandle
                  }
          (_, _, _, ph) <- createProcess process
          exitCode <- waitForProcess ph
          log Info $ "Task " <> show taskId <> " finished with exit code " <> show exitCode
          h pwd exitCode
          pure exitCode
        pure (Map.insert taskId task tasks, pwd)

-- | Kill a task
killTask :: TaskSupervisor -> TaskId -> Eff App.AppStack ()
killTask supervisor taskId = do
  log Info $ "Killing task " <> show taskId
  modifyMVar_ (tasks supervisor) $ \tasks -> do
    for_ (Map.lookup taskId tasks) cancel
    pure $ Map.delete taskId tasks

-- | Get the status of a task
getTaskStatus :: (Concurrent :> es) => TaskSupervisor -> TaskId -> Eff es TaskState
getTaskStatus supervisor taskId = do
  tasks <- readMVar (tasks supervisor)
  case Map.lookup taskId tasks of
    Nothing -> pure Killed
    Just task -> do
      status <- poll task
      case status of
        Nothing -> pure Running
        Just (Right output) -> pure $ Finished output
        Just (Left _) -> pure Killed
