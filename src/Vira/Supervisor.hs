{-# HLINT ignore "Use next" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vira.Supervisor where

import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar, modifyMVar_, readMVar)
import Effectful.FileSystem (FileSystem, createDirectory)
import Effectful.FileSystem.IO (hClose, openFile)
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
  -- The working directory of the job (will be created)
  FilePath ->
  -- The shell command to run
  String ->
  -- Handler to call after the task finishes
  ( -- Exit code
    ExitCode ->
    Eff es ()
  ) ->
  Eff es ()
startTask supervisor taskId pwd cmd h = do
  logSupervisorState supervisor
  log Info $ "Starting task: " <> toText cmd
  modifyMVar (tasks supervisor) $ \tasks -> do
    if Map.member taskId tasks
      then do
        log Error $ "Task " <> show taskId <> " already exists"
        die $ "Task " <> show taskId <> " already exists"
      else do
        createDirectory pwd
        asyncHandle <- Effectful.Concurrent.Async.async $ startTask' taskId pwd cmd h
        let task = Task {workDir = pwd, asyncHandle}
        pure (Map.insert taskId task tasks, ())

startTask' ::
  (Process :> es, Log Message :> es, IOE :> es, FileSystem :> es) =>
  TaskId ->
  FilePath ->
  String ->
  (ExitCode -> Eff es ()) ->
  Eff es ExitCode
startTask' taskId pwd cmd h = do
  -- Send all output to a file under working directory.
  -- Write vira level log entry to the output log
  let outputLogFile = pwd </> "output.log"
  -- TODO: In lieu of https://github.com/juspay/vira/issues/6
  let buildLog (msg :: Text) = do
        let s = "[vira:job:" <> show taskId <> "] " <> msg <> "\n"
        appendFileText outputLogFile s
  buildLog $ "Task started: " <> toText cmd
  outputHandle <- openFile outputLogFile AppendMode
  let processSettings s =
        s
          { cwd = Just pwd
          , std_out = UseHandle outputHandle
          , std_err = UseHandle outputHandle
          }
      process =
        -- FIXME: Using `shell` is not considered secure.
        shell cmd & processSettings
  (_, _, _, ph) <- createProcess process
  exitCode <- waitForProcess ph
  let msg = "Task " <> show taskId <> " finished with exit code " <> show exitCode
  log Info msg
  buildLog msg
  hClose outputHandle
  h exitCode
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
