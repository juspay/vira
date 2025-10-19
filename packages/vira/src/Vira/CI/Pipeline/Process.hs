{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Process (
  runProcess,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, tagCurrentThread, withLogContext)
import Effectful.Colog.Simple.Process (withLogCommand)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catch, finally, mask)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (hClose, openFile)
import Effectful.Process (CreateProcess (create_group, cwd, std_err, std_out), Process, StdStream (UseHandle), createProcess, getPid, interruptProcessGroupOf, waitForProcess)
import Effectful.Reader.Static qualified as ER
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Vira.CI.Error (PipelineError (PipelineProcessFailed, PipelineTerminated))
import Vira.CI.Pipeline.Effect (PipelineEnv (logger), PipelineLogger (unPipelineLogger))
import Vira.Supervisor.Type (Terminated (Terminated))

-- | Helper: Run a single process with logger from effect
runProcess ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  Maybe FilePath ->
  CreateProcess ->
  Eff es ()
runProcess repoDir outputLog p = do
  env <- ER.ask @PipelineEnv
  runProcess' repoDir outputLog (unPipelineLogger env.logger) (one p) >>= \case
    Left err -> throwError $ PipelineTerminated err
    Right ExitSuccess -> pass
    Right exitCode -> throwError $ PipelineProcessFailed exitCode

{- | Run processes sequentially in the given working directory.

Each process runs one after another, stopping at the first non-zero exit code.
Output is redirected to the file specified by outputFile (if provided), otherwise
it goes to stdout/stderr. The logger callback handles user-facing pipeline messages.

Returns 'Left Terminated' if interrupted by async exception, or 'Right ExitCode'
when all processes complete (with the exit code of the first failure, or ExitSuccess).
-}
runProcess' ::
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
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  -- | Processes to run in sequence
  NonEmpty CreateProcess ->
  Eff es (Either Terminated ExitCode)
runProcess' workDir mOutputFile taskLogger procs = do
  tagCurrentThread "🛞 "
  runProcs $ toList procs
  where
    -- Run each process one after another; exiting immediately if any fails
    runProcs :: [CreateProcess] -> Eff es (Either Terminated ExitCode)
    runProcs [] = do
      taskLogger Info "All procs for task finished successfully"
      pure $ Right ExitSuccess
    runProcs (process : rest) =
      runProc process >>= \case
        Right ExitSuccess -> do
          runProcs rest
        x -> pure x

    runProc :: CreateProcess -> Eff es (Either Terminated ExitCode)
    runProc process = do
      withLogCommand process $ do
        taskLogger Info "Starting task"
        case mOutputFile of
          Nothing -> runProcWithSettings process id
          Just outputFile ->
            withFileHandle outputFile AppendMode $ \outputHandle ->
              runProcWithSettings process (redirectOutputTo outputHandle)

    runProcWithSettings :: CreateProcess -> (CreateProcess -> CreateProcess) -> Eff es (Either Terminated ExitCode)
    runProcWithSettings process extraSettings = do
      let processSettings =
            extraSettings
              >>> (\cp -> cp {cwd = Just workDir, create_group = True})
      (_, _, _, ph) <- createProcess $ process & processSettings
      pid <- getPid ph
      withLogContext [("pid", maybe "?" show pid)] $ do
        taskLogger Debug "Task spawned"

        result <-
          -- `mask` cleanup from asynchronous interruptions
          mask $ \restore ->
            restore (Right <$> waitForProcess ph)
              `catch` \case
                Terminated -> do
                  taskLogger Info "Terminating process"
                  interruptProcessGroupOf ph `catch` \(e :: SomeException) ->
                    -- Analogous to `Control-C`'ing the process in an interactive shell
                    taskLogger Error $ "Failed to terminate process: " <> show e
                  _ <- waitForProcess ph -- Reap to prevent zombies
                  pure $ Left Terminated
        case result of
          Right ExitSuccess -> taskLogger Info "Task completed successfully"
          Right (ExitFailure code) -> taskLogger Error $ "Task failed with exit code " <> show code
          Left err -> taskLogger Error $ toText $ displayException err
        pure result

-- | With stdout and stderr redirected to given handle
redirectOutputTo :: Handle -> CreateProcess -> CreateProcess
redirectOutputTo h p =
  p
    { std_out = UseHandle h
    , std_err = UseHandle h
    }

-- | Helper function that provides withFile-like behavior for Effectful
withFileHandle :: (FileSystem :> es, IOE :> es) => FilePath -> IOMode -> (Handle -> Eff es a) -> Eff es a
withFileHandle path mode action = do
  handle <- openFile path mode
  finally (action handle) (hClose handle)
