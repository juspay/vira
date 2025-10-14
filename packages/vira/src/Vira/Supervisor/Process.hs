module Vira.Supervisor.Process (
  runProcesses,
) where

import Colog (Message, Severity (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Exception (catch, finally, mask)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (hClose, openFile)
import Effectful.Process (CreateProcess (cmdspec, create_group), Pid, Process, createProcess, getPid, interruptProcessGroupOf, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Vira.Lib.Logging (log, tagCurrentThread)
import Vira.Lib.Process qualified as Process
import Vira.Supervisor.Type (TaskId, Terminated (Terminated))

-- | Run a sequence of processes, stopping on first failure
runProcesses ::
  forall es.
  ( Concurrent :> es
  , Process :> es
  , Log Message :> es
  , IOE :> es
  , FileSystem :> es
  ) =>
  TaskId ->
  FilePath ->
  (forall es1. (IOE :> es1) => Text -> Eff es1 ()) ->
  -- List of processes to run in sequence
  NonEmpty CreateProcess ->
  Eff es (Either Terminated ExitCode)
runProcesses taskId workDir logger procs = do
  tagCurrentThread $ "🪜 ;task=" <> show taskId
  runProcs $ toList procs
  where
    -- Run each process one after another; exiting immediately if any fails
    runProcs :: [CreateProcess] -> Eff es (Either Terminated ExitCode)
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

    runProc :: CreateProcess -> Eff es (Maybe Pid, Either Terminated ExitCode)
    runProc process = do
      tagCurrentThread $ "🪜 ;task=" <> show taskId
      log Debug $ "Starting task: " <> show (cmdspec process)
      logger $ "Starting task: " <> show (cmdspec process)
      withFileHandle (outputLogFile workDir) AppendMode $ \outputHandle -> do
        let processSettings =
              Process.alwaysUnderPath workDir
                >>> Process.redirectOutputTo outputHandle
                >>> (\cp -> cp {create_group = True}) -- For `interruptProcessGroupOf`, when the process is `Terminated`
        (_, _, _, ph) <- createProcess $ process & processSettings
        pid <- getPid ph
        tagCurrentThread $ "🪜 ;task=" <> show taskId <> ";pid=" <> maybe "?" show pid
        log Info $ "Task spawned : " <> show (cmdspec process)

        result <-
          -- `mask` cleanup from asynchronous interruptions
          mask $ \restore ->
            restore (Right <$> waitForProcess ph)
              `catch` \case
                Terminated -> do
                  log Info "Terminating process"
                  interruptProcessGroupOf ph `catch` \(e :: SomeException) ->
                    -- Analogous to `Control-C`'ing the process in an interactive shell
                    log Error $ "Failed to terminate process: " <> show e
                  _ <- waitForProcess ph -- Reap to prevent zombies
                  pure $ Left Terminated
        log Debug $ "Task finished: " <> show (cmdspec process)
        logger $
          "A task (pid=" <> show pid <> ") finished with " <> either (("exception: " <>) . toText . displayException) (("exitcode: " <>) . show) result
        log Debug "Workspace log done"
        pure (pid, result)

-- Send all output to a file under working directory.
outputLogFile :: FilePath -> FilePath
outputLogFile base = base </> "output.log"

-- | Helper function that provides withFile-like behavior for Effectful
withFileHandle :: (FileSystem :> es, IOE :> es) => FilePath -> IOMode -> (Handle -> Eff es a) -> Eff es a
withFileHandle path mode action = do
  handle <- openFile path mode
  finally (action handle) (hClose handle)
