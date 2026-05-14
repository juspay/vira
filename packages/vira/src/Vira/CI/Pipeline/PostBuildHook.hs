{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Post-build hook: exec a single operator-configured shell script after a successful pipeline.
module Vira.CI.Pipeline.PostBuildHook (
  -- * Effect implementation
  postBuildImpl,

  -- * Used in tests
  HookError (..),
  displayHookError,
  hookEnvVars,
  runHook,
) where

import Prelude hiding (asks)

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception qualified as CE
import Data.Map qualified as Map
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static qualified as ER
import LogSink (Sink (..))
import LogSink.Handle (drainHandleWith)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (CreatePipe), interruptProcessGroupOf, proc, waitForProcess, withCreateProcess)
import System.Timeout qualified as Timeout
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Error (PipelineError, pipelineToolError)
import Vira.CI.Pipeline.Effect (PipelineEnv (..), logPipeline)

{- | Environment variables passed to the post-build hook.

Always emits @VIRA_BRANCH@ and @VIRA_COMMIT_ID@. Emits
@VIRA_REPO_CLONE_URL@ (the full clone URL) when an origin remote is
configured — operator scripts dispatch on this exact value to avoid
short-name collisions across orgs.
-}
hookEnvVars :: ViraContext -> [(Text, Text)]
hookEnvVars ctx =
  [ ("VIRA_BRANCH", toText ctx.branch)
  , ("VIRA_COMMIT_ID", toText ctx.commitId)
  ]
    <> maybe [] (\url -> [("VIRA_REPO_CLONE_URL", url)]) ctx.cloneUrl

-- | Failure modes for 'runHook'.
data HookError
  = -- | Hook process exited non-zero with the given status code.
    HookExited Int
  | -- | Hook process was killed after running longer than the timeout (seconds).
    HookTimedOut Int
  | -- | @CreatePipe@ on stdout/stderr unexpectedly produced no handle.
    HookMissingHandle
  deriving stock (Show, Eq)

-- | Render a 'HookError' for log output.
displayHookError :: HookError -> Text
displayHookError = \case
  HookExited code -> "Hook script exited with code " <> show code
  HookTimedOut secs -> "Hook script timed out after " <> show secs <> "s"
  HookMissingHandle -> "Hook script produced no stdout/stderr handle"

{- | Execute a shell script with environment variables.

Subprocess stdout/stderr are streamed line by line to @sink@. The hook
is killed (process group) if it runs longer than @timeoutMicros@.
Returns 'HookError' when the script exits non-zero, times out, or the
runtime fails to attach the expected pipe handles.
-}
runHook ::
  -- | Path to the shell script to execute
  FilePath ->
  -- | Environment variables to set; override any same-named inherited vars
  [(Text, Text)] ->
  -- | Working directory
  FilePath ->
  -- | Sink for subprocess output
  Sink Text ->
  -- | Timeout in microseconds; 'Nothing' disables the timeout
  Maybe Int ->
  IO (Either HookError ())
runHook scriptPath envVars workDir sink mTimeoutMicros = do
  -- VIRA_* values take precedence over any same-named inherited vars,
  -- and Map.union deduplicates so execve receives a single value per key.
  currentEnv <- getEnvironment
  let processEnv =
        Map.toList $
          Map.fromList (map (bimap toString toString) envVars)
            `Map.union` Map.fromList currentEnv
      cp =
        (proc scriptPath [])
          { env = Just processEnv
          , cwd = Just workDir
          , std_out = CreatePipe
          , std_err = CreatePipe
          , create_group = True
          }
  withCreateProcess cp $ \_ mStdoutH mStderrH ph ->
    case (mStdoutH, mStderrH) of
      (Just stdoutH, Just stderrH) ->
        withAsync (drainHandleWith identity stdoutH sink) $ \stdoutAsync ->
          withAsync (drainHandleWith identity stderrH sink) $ \stderrAsync -> do
            mExit <- case mTimeoutMicros of
              Nothing -> Just <$> waitForProcess ph
              Just t -> Timeout.timeout t (waitForProcess ph)
            wait stdoutAsync
            wait stderrAsync
            case mExit of
              Nothing -> do
                interruptProcessGroupOf ph `CE.catch` \(_ :: SomeException) -> pass
                _ <- waitForProcess ph
                let secs = maybe 0 (`div` 1_000_000) mTimeoutMicros
                pure $ Left $ HookTimedOut secs
              Just ExitSuccess -> pure $ Right ()
              Just (ExitFailure code) -> pure $ Left $ HookExited code
      _ -> pure $ Left HookMissingHandle

-- | Implementation of the @PostBuild@ branch of the 'Vira.CI.Pipeline.Effect.Pipeline' effect.
postBuildImpl ::
  ( Log (RichMessage IO) :> es
  , IOE :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  Eff es ()
postBuildImpl = do
  env <- ER.ask @PipelineEnv
  case env.postBuildHook of
    Nothing -> logPipeline Info "No post-build hook configured, skipping"
    Just scriptPath -> do
      let ctx = env.viraContext
      logPipeline Info $ "Running post-build hook: " <> toText scriptPath
      result <-
        liftIO $
          runHook
            scriptPath
            (hookEnvVars ctx)
            ctx.repoDir
            env.logSink
            (Just hookTimeoutMicros)
      case result of
        Left err ->
          throwError $
            pipelineToolError
              ("Post-build hook failed: " <> displayHookError err)
              (Nothing :: Maybe Text)
        Right () ->
          logPipeline Info "Post-build hook succeeded"

-- | Maximum hook runtime before vira kills the subprocess (5 minutes).
hookTimeoutMicros :: Int
hookTimeoutMicros = 300 * 1_000_000
