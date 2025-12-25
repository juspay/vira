{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Process (
  runProcess,
  runProcessWithContext,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Control.Concurrent.Async (wait, withAsync)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), tagCurrentThread, withLogContext)
import Effectful.Colog.Simple.Process (withLogCommand)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catch, finally, mask)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (hClose, openFile)
import Effectful.Process (CreateProcess (create_group, cwd, std_err, std_out), Process, ProcessHandle, StdStream (CreatePipe, UseHandle), createProcess, getPid, interruptProcessGroupOf, waitForProcess)
import Effectful.Reader.Static qualified as ER
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hGetLine, hPutStrLn)
import System.IO.Error (isEOFError, tryIOError)
import System.Process qualified as P
import Vira.CI.Error (PipelineError (PipelineProcessFailed, PipelineTerminated))
import Vira.CI.Log (ViraLog (..), encodeViraLog)
import Vira.CI.Pipeline.Effect (PipelineEnv (logger, outputLog), PipelineLogger (unPipelineLogger))
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
  runProcess' repoDir env.outputLog (unPipelineLogger env.logger) Nothing p >>= \case
    Left err -> throwError $ PipelineTerminated err
    Right ExitSuccess -> pass
    Right exitCode -> throwError $ PipelineProcessFailed exitCode

{- | Run a process with explicit build context (flake/system)
Output is streamed line-by-line and wrapped in ViraLog JSON format
-}
runProcessWithContext ::
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
  LogContext -> -- Build context (e.g., flake, system)
  CreateProcess ->
  Eff es ()
runProcessWithContext repoDir buildCtx p = do
  env <- ER.ask @PipelineEnv
  runProcess' repoDir env.outputLog (unPipelineLogger env.logger) (Just buildCtx) p >>= \case
    Left err -> throwError $ PipelineTerminated err
    Right ExitSuccess -> pass
    Right exitCode -> throwError $ PipelineProcessFailed exitCode

{- | Run a single process in the given working directory.

Output is redirected to the file specified by outputFile (if provided), otherwise
it goes to stdout/stderr. The logger callback handles user-facing pipeline messages.

When buildContext is provided, output is streamed line-by-line and wrapped in ViraLog JSON.

Returns 'Left Terminated' if interrupted by async exception, or 'Right ExitCode'
with the process exit code.
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
  -- | Working directory for process
  FilePath ->
  -- | Optional output log file path
  Maybe FilePath ->
  -- | Logger callback for user-facing messages
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  -- | Optional build context for line-by-line JSON wrapping
  Maybe LogContext ->
  -- | Process to run
  CreateProcess ->
  Eff es (Either Terminated ExitCode)
runProcess' workDir mOutputFile taskLogger mBuildCtx process = do
  tagCurrentThread "🛞 "
  withLogCommand process $ do
    taskLogger Info "Starting task"
    case (mOutputFile, mBuildCtx) of
      (Just outputFile, Just buildCtx) ->
        -- Stream output with JSON wrapping
        withFileHandle outputFile AppendMode $ \outputHandle ->
          runProcWithStreaming outputHandle buildCtx
      (Just outputFile, Nothing) ->
        -- Direct output redirection (legacy mode)
        withFileHandle outputFile AppendMode $ \outputHandle ->
          runProcWithSettings (redirectOutputTo outputHandle)
      (Nothing, _) ->
        -- No output file, just run normally
        runProcWithSettings id
  where
    runProcWithSettings :: (CreateProcess -> CreateProcess) -> Eff es (Either Terminated ExitCode)
    runProcWithSettings extraSettings = do
      let processSettings =
            extraSettings
              >>> (\cp -> cp {cwd = Just workDir, create_group = True})
      (_, _, _, ph) <- createProcess $ process & processSettings
      pid <- getPid ph
      withLogContext [("pid", maybe "?" show pid)] $ do
        taskLogger Debug "Task spawned"
        waitAndHandle ph

    runProcWithStreaming :: Handle -> LogContext -> Eff es (Either Terminated ExitCode)
    runProcWithStreaming outputHandle buildCtx = do
      let processSettings cp =
            cp
              { cwd = Just workDir
              , create_group = True
              , std_out = CreatePipe
              , std_err = CreatePipe
              }
      (_, mStdout, mStderr, ph) <- createProcess $ process & processSettings
      pid <- getPid ph
      withLogContext [("pid", maybe "?" show pid)] $ do
        taskLogger Debug "Task spawned (streaming mode)"
        case (mStdout, mStderr) of
          (Just hOut, Just hErr) -> do
            -- Stream both pipes concurrently to avoid deadlock
            liftIO $ do
              hSetBuffering hOut LineBuffering
              hSetBuffering hErr LineBuffering
              withAsync (streamLines hOut outputHandle buildCtx) $ \aOut ->
                withAsync (streamLines hErr outputHandle buildCtx) $ \aErr -> do
                  -- Wait for process first
                  exitCode <- waitForProcessIO ph
                  -- Then wait for all output to be captured
                  wait aOut
                  wait aErr
                  pure exitCode
          _ -> do
            taskLogger Error "Failed to create pipes"
            waitAndHandle ph

    waitAndHandle :: Effectful.Process.ProcessHandle -> Eff es (Either Terminated ExitCode)
    waitAndHandle ph = do
      result <-
        mask $ \restore ->
          restore (Right <$> waitForProcess ph)
            `catch` \case
              Terminated -> do
                taskLogger Info "Terminating process"
                interruptProcessGroupOf ph `catch` \(e :: SomeException) ->
                  taskLogger Error $ "Failed to terminate process: " <> show e
                _ <- waitForProcess ph
                pure $ Left Terminated
      case result of
        Right ExitSuccess -> taskLogger Info "Task completed successfully"
        Right (ExitFailure code) -> taskLogger Error $ "Task failed with exit code " <> show code
        Left err -> taskLogger Error $ toText $ displayException err
      pure result

    -- IO version for use within withAsync
    waitForProcessIO :: ProcessHandle -> IO (Either Terminated ExitCode)
    waitForProcessIO ph = Right <$> P.waitForProcess ph

-- | Stream lines from input handle, wrap in ViraLog JSON, write to output
streamLines :: Handle -> Handle -> LogContext -> IO ()
streamLines input output ctx = loop
  where
    loop = do
      mLine <- tryIOError $ hGetLine input
      case mLine of
        Left err
          | isEOFError err -> pass -- Normal EOF, stop
          | otherwise -> do
              -- Log the error
              let errMsg :: String
                  errMsg = show err
                  errLog = ViraLog Warning (toText errMsg) ctx
              hPutStrLn output (toString $ encodeViraLog errLog)
        Right line -> do
          let viraLog = ViraLog Debug (toText line) ctx
          hPutStrLn output (toString $ encodeViraLog viraLog)
          loop

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
