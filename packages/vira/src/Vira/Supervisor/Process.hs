module Vira.Supervisor.Process (
  runProcesses,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log, tagCurrentThread)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Exception (catch, finally, mask)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (hClose, openFile)
import Effectful.Process (CreateProcess (cmdspec, create_group), Pid, Process, createProcess, getPid, interruptProcessGroupOf, waitForProcess)
import Effectful.Reader.Static qualified as ER
import System.Exit (ExitCode (ExitSuccess))
import Vira.Lib.Process qualified as Process
import Vira.Supervisor.Type (Terminated (Terminated))

{- | Run a sequence of processes sequentially in the given working directory.

Processes run one after another, stopping on the first non-zero exit code.
If outputFile is Just path, process output is logged to that file.
If outputFile is Nothing, process output goes to stdout/stderr.
Returns 'Left Terminated' if interrupted, or 'Right ExitCode' on completion.
-}
runProcesses ::
  forall es.
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  ) =>
  -- | Working directory for processes
  FilePath ->
  -- | Optional output log file path
  Maybe FilePath ->
  -- | Logger callback for user-facing messages
  (forall es1. (IOE :> es1) => Text -> Eff es1 ()) ->
  -- | Processes to run in sequence
  NonEmpty CreateProcess ->
  Eff es (Either Terminated ExitCode)
runProcesses workDir mOutputFile taskLogger procs = do
  tagCurrentThread "🛞 "
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
      log Debug $ "Starting task: " <> show (cmdspec process)
      taskLogger $ "Starting task: " <> show (cmdspec process)
      case mOutputFile of
        Nothing -> runProcWithSettings process id
        Just outputFile ->
          withFileHandle outputFile AppendMode $ \outputHandle ->
            runProcWithSettings process (Process.redirectOutputTo outputHandle)

    runProcWithSettings :: CreateProcess -> (CreateProcess -> CreateProcess) -> Eff es (Maybe Pid, Either Terminated ExitCode)
    runProcWithSettings process extraSettings = do
      let processSettings =
            Process.alwaysUnderPath workDir
              >>> extraSettings
              >>> (\cp -> cp {create_group = True}) -- For `interruptProcessGroupOf`, when the process is `Terminated`
      (_, _, _, ph) <- createProcess $ process & processSettings
      pid <- getPid ph
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
      taskLogger $
        "A task (pid=" <> show pid <> ") finished with " <> either (("exception: " <>) . toText . displayException) (("exitcode: " <>) . show) result
      log Debug "Workspace log done"
      pure (pid, result)

-- | Helper function that provides withFile-like behavior for Effectful
withFileHandle :: (FileSystem :> es, IOE :> es) => FilePath -> IOMode -> (Handle -> Eff es a) -> Eff es a
withFileHandle path mode action = do
  handle <- openFile path mode
  finally (action handle) (hClose handle)
