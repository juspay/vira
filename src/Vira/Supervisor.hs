{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use next" #-}
module Vira.Supervisor where

import Data.Map.Strict qualified as Map
import Effectful (Eff, (:>))
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar, modifyMVar_, readMVar)
import Effectful.Process (Process, readProcessWithExitCode)
import Vira.App qualified as App
import Vira.App.Logging
import Vira.Supervisor.Type
import Prelude hiding (readMVar)

newSupervisor :: (MonadIO m) => m TaskSupervisor
newSupervisor = do
  tasks <- newMVar mempty
  pure $ TaskSupervisor tasks ()

logSupervisorState :: (HasCallStack, Concurrent :> es, Log Message :> es) => TaskSupervisor -> Eff es ()
logSupervisorState supervisor = do
  tasks <- readMVar (tasks supervisor)
  withFrozenCallStack $ log Debug $ "Current tasks: " <> show (Map.keys tasks)
  forM_ (Map.toList tasks) $ \(taskId, _) -> do
    taskState <- getTaskStatus supervisor taskId
    withFrozenCallStack $ log Debug $ "Task " <> show taskId <> " state: " <> show taskState

-- | Start a new task
startTask ::
  (Concurrent :> es, Process :> es, Log Message :> es, HasCallStack) =>
  TaskSupervisor ->
  TaskId ->
  String ->
  -- Handler to call after the task finishes
  (TaskOutput -> Eff es ()) ->
  Eff es TaskId
startTask supervisor taskId cmd h = do
  logSupervisorState supervisor
  log Info $ "Starting task: " <> toText cmd
  modifyMVar (tasks supervisor) $ \tasks -> do
    if Map.member taskId tasks
      then do
        log Error $ "Task " <> show taskId <> " already exists"
        pure (tasks, taskId)
      else do
        task <- async $ do
          (exitCode, output, _) <- readProcessWithExitCode "sh" ["-c", cmd <> " 2>&1"] ""
          log Info $ "Task " <> show taskId <> " finished with exit code " <> show exitCode
          let out = TaskOutput output exitCode
          h out
          pure out
        pure (Map.insert taskId task tasks, taskId)

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
