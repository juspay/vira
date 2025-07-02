{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Use next" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vira.Supervisor where

import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar, modifyMVar_, readMVar)
import Effectful.Exception (catch, finally, mask)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.FileSystem.IO (hClose, openFile)
import Effectful.Process (CreateProcess (cmdspec, create_group), Pid, Process, createProcess, getPid, interruptProcessGroupOf, waitForProcess)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Directory qualified
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
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
    Either TaskException ExitCode ->
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
        logToWorkspaceOutput taskId pwd msg
        asyncHandle <- async $ do
          hdl <- startTask' taskId pwd h procs
          logToWorkspaceOutput taskId pwd "CI finished"
          pure hdl
        let task = Task {workDir = pwd, asyncHandle}
        pure (Map.insert taskId task tasks, ())

-- Send all output to a file under working directory.
-- Write vira level log entry to the output log
outputLogFile :: FilePath -> FilePath
outputLogFile base = base </> "output.log"

-- TODO: In lieu of https://github.com/juspay/vira/issues/6
logToWorkspaceOutput :: (IOE :> es) => TaskId -> FilePath -> Text -> Eff es ()
logToWorkspaceOutput taskId base (msg :: Text) = do
  let s = "🥕 [vira:job:" <> show taskId <> "] " <> msg <> "\n"
  appendFileText (outputLogFile base) s

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

-- | Helper function that provides withFile-like behavior for Effectful
withFileHandle :: (FileSystem :> es, IOE :> es) => FilePath -> IOMode -> (Handle -> Eff es a) -> Eff es a
withFileHandle path mode action = do
  handle <- openFile path mode
  finally (action handle) (hClose handle)
