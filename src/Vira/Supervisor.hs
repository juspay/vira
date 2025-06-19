{-# HLINT ignore "Use next" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vira.Supervisor where

import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar (modifyMVar, modifyMVar_, readMVar)
import Effectful.Exception (catch, finally, throwIO, uninterruptibleMask_)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.FileSystem.IO (hClose, openFile)
import Effectful.Process (CreateProcess (cmdspec), Pid, Process, ProcessHandle, createProcess, getPid, terminateProcess, waitForProcess)
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
        currentProcHandle <- newMVar Nothing
        asyncHandle <-
          async $
            ( do
                exitCode <- startTask' taskId pwd currentProcHandle procs
                h (Right exitCode)
                logToWorkspaceOutput taskId pwd "CI finished"
                pure exitCode
            )
              `catch` \(e :: TaskException) -> do
                h (Left e)
                throwIO e -- Re-throw to ensure Async state reflects cancellation
        let task = Task {workDir = pwd, asyncHandle, currentProcHandle}
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
  MVar (Maybe ProcessHandle) ->
  -- List of processes to run in sequence
  NonEmpty CreateProcess ->
  Eff es ExitCode
startTask' taskId pwd cph = flip runProcs cph . toList
  where
    -- Run each process one after another; exiting immediately if any fails
    runProcs :: [CreateProcess] -> MVar (Maybe ProcessHandle) -> Eff es ExitCode
    runProcs [] _ = do
      log Info $ "All procs for task " <> show taskId <> " finished successfully"
      pure ExitSuccess
    runProcs (proc : rest) currentProcHandle =
      runProc proc currentProcHandle >>= \case
        (pid, ExitSuccess) -> do
          log Debug $ "A proc for task " <> show taskId <> " (pid=" <> show pid <> ") successfully finished."
          runProcs rest currentProcHandle
        (pid, exitCode) -> do
          log Warning $ "A proc for task " <> show taskId <> " (pid=" <> show pid <> ") failed with exitCode " <> show exitCode
          pure exitCode

    runProc :: CreateProcess -> MVar (Maybe ProcessHandle) -> Eff es (Maybe Pid, ExitCode)
    runProc proc currentProcHandle = do
      log Debug $ "Starting task: " <> show (cmdspec proc)
      logToWorkspaceOutput taskId pwd $ "Starting task: " <> show (cmdspec proc)
      outputHandle <- openFile (outputLogFile pwd) AppendMode
      let processSettings =
            Process.alwaysUnderPath pwd
              >>> Process.redirectOutputTo outputHandle
      (_, _, _, ph) <- createProcess $ proc & processSettings
      modifyMVar_ currentProcHandle (\_ -> pure $ Just ph)
      pid <- getPid ph
      log Debug $ "Task spawned (pid=" <> show pid <> "): " <> show (cmdspec proc)

      -- `uninterruptibleMask_ ensures `waitForProcess` is not interrupted by async exceptions. This is crucial for preventing the process from becoming a zombie
      --
      -- NOTE: Use `uninterruptibleMask_` with caution, see <https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Exception.html#v:uninterruptibleMask>
      exitCode <-
        uninterruptibleMask_ $
          waitForProcess ph `finally` modifyMVar_ currentProcHandle (\_ -> pure Nothing)

      log Debug $ "Task finished (pid=" <> show pid <> "): " <> show (cmdspec proc)
      hClose outputHandle
      logToWorkspaceOutput taskId pwd $ "A task (pid=" <> show pid <> ") finished with exit code " <> show exitCode
      log Debug "Workspace log done"
      pure (pid, exitCode)

-- | Kill a task
killTask :: (Concurrent :> es, Process :> es, Log Message :> es, IOE :> es) => TaskSupervisor -> TaskId -> Eff es ()
killTask supervisor taskId = do
  log Info $ "Killing task " <> show taskId
  modifyMVar_ (tasks supervisor) $ \tasks -> do
    for_ (Map.lookup taskId tasks) $ \Task {..} -> do
      ph <- readMVar currentProcHandle
      case ph of
        Nothing -> log Info $ "No active process found for task " <> show taskId
        Just ph' -> do
          pid <- getPid ph'
          log Info $ "Terminating active process (pid=" <> show pid <> ") for task " <> show taskId
          terminateProcess ph'
      cancelWith asyncHandle UserKilled
    pure $ Map.delete taskId tasks

taskState :: (Concurrent :> es) => Task -> Eff es TaskState
taskState Task {..} = do
  status <- poll asyncHandle
  case status of
    Nothing -> pure Running
    Just (Right _) -> pure $ Finished ExitSuccess
    Just (Left _) -> pure Killed
