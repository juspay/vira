{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Process (
  runProcess,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception qualified as CE
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), tagCurrentThread, withLogContext)
import Effectful.Colog.Simple.Process (formatCommandForLog)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.Process (CreateProcess (create_group, cwd, std_err, std_out), Process, StdStream (CreatePipe), createProcess)
import Effectful.Reader.Static qualified as ER
import LogSink (Sink (..))
import LogSink.Handle (drainHandleWith)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (interruptProcessGroupOf, waitForProcess)
import Vira.CI.Error (PipelineError (PipelineProcessFailed, PipelineTerminated))
import Vira.CI.Pipeline.Effect (PipelineEnv (..), logPipeline)
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
  CreateProcess ->
  Eff es ()
runProcess repoDir p = do
  env <- ER.ask @PipelineEnv
  runProcess' repoDir env.logSink p >>= \case
    Left err -> throwError $ PipelineTerminated err
    Right ExitSuccess -> pass
    Right exitCode -> throwError $ PipelineProcessFailed exitCode

{- | Run a single process in the given working directory.

Subprocess stdout/stderr and task lifecycle messages are both written to the logSink.
ViraLog messages are written as JSON, raw subprocess output as-is.

Returns 'Left Terminated' if interrupted by async exception, or 'Right ExitCode'
with the process exit code.
-}
runProcess' ::
  forall es.
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , IOE :> es
  , FileSystem :> es
  ) =>
  -- | Working directory for process
  FilePath ->
  -- | Log sink for all output (ViraLog JSON + subprocess raw output)
  Sink Text ->
  -- | Process to run
  CreateProcess ->
  Eff es (Either Terminated ExitCode)
runProcess' workDir logSink process = do
  tagCurrentThread "🛞 "
  withLogContext [("cmd", formatCommandForLog process)] $ do
    logPipeline Info "Starting task"
    runProcWithSink logSink
  where
    runProcWithSink :: Sink Text -> Eff es (Either Terminated ExitCode)
    runProcWithSink sink = do
      let processSettings cp =
            cp {cwd = Just workDir, create_group = True, std_out = CreatePipe, std_err = CreatePipe}
      (_, mStdoutH, mStderrH, ph) <- createProcess $ process & processSettings
      let stdoutH = fromMaybe (error "Expected stdout handle") mStdoutH
          stderrH = fromMaybe (error "Expected stderr handle") mStderrH
      logPipeline Debug "Task spawned"

      -- Spawn drain threads in IO to capture subprocess output
      -- They will naturally terminate when handles are closed at process exit
      result <- liftIO $ withAsync (drainHandleWith id stdoutH sink) $ \stdoutAsync ->
        withAsync (drainHandleWith id stderrH sink) $ \stderrAsync -> do
          -- Wait for process, handling interruption
          procResult <- CE.mask $ \restore ->
            restore (Right <$> waitForProcess ph)
              `CE.catch` \case
                Terminated -> do
                  interruptProcessGroupOf ph `CE.catch` \(e :: SomeException) ->
                    sink.sinkWrite $ "Failed to terminate process: " <> show e
                  _ <- waitForProcess ph -- Reap to prevent zombies
                  pure $ Left Terminated
          -- Wait for drains to complete (handles closed by waitForProcess)
          wait stdoutAsync
          wait stderrAsync
          pure procResult

      case result of
        Right ExitSuccess -> logPipeline Info "Task completed successfully"
        Right (ExitFailure code) -> logPipeline Error $ "Task failed with exit code " <> show code
        Left err -> logPipeline Error $ toText $ displayException err
      pure result
