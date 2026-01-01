{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline.Effect where

import Prelude hiding (asks)

import Colog (Severity)
import DevourFlake (DevourFlakeResult)
import Effectful (Eff, Effect, IOE, type (:>))
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Reader.Static qualified as ER
import Effectful.TH
import LogSink (Sink (..))
import System.FilePath ((</>))
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Log (ViraLog (..), decodeViraLog, encodeViraLog, renderViraLogCLI)
import Vira.CI.Pipeline.Type (ViraPipeline)
import Vira.Environment.Tool.Type.Tools (Tools)
import Vira.State.Type (Branch (..), Repo)

-- | Environment for 'Pipeline' execution
data PipelineEnv = PipelineEnv
  { outputLog :: Maybe FilePath
  -- ^ Optional output log file
  , tools :: Tools
  -- ^ Available CI 'Vira.Environment.Tool.Type.Tools.Tools'
  , viraContext :: ViraContext
  -- ^ 'ViraContext' (branch, onlyBuild flag)
  , logSink :: Sink Text
  -- ^ 'LogSink.Sink' for all output (ViraLog JSON + subprocess raw output)
  }
  deriving stock (Generic)

-- | Helper: Write a ViraLog message to a sink
writePipelineLog ::
  (IOE :> es, ER.Reader LogContext :> es) =>
  Sink Text ->
  Severity ->
  Text ->
  Eff es ()
writePipelineLog = writePipelineLogFiltered []

{- | Helper: Write a ViraLog message to a sink, filtering out specified context keys

Useful for workspace-scoped logs where context like repo/branch/job is
already encoded in the file path and would be redundant.
-}
writePipelineLogFiltered ::
  (IOE :> es, ER.Reader LogContext :> es) =>
  -- | Context keys to exclude from the log entry
  [Text] ->
  Sink Text ->
  Severity ->
  Text ->
  Eff es ()
writePipelineLogFiltered excludeKeys sink severity msg = do
  LogContext logCtx <- ER.ask @LogContext
  let filteredCtx = LogContext $ filter ((`notElem` excludeKeys) . fst) logCtx
  let viraLog = ViraLog {level = severity, message = msg, context = filteredCtx}
  liftIO $ sinkWrite sink (encodeViraLog viraLog)

-- | Helper: Log a pipeline message using 'logSink' from 'PipelineEnv'
logPipeline ::
  ( ER.Reader PipelineEnv :> es
  , ER.Reader LogContext :> es
  , IOE :> es
  ) =>
  Severity ->
  Text ->
  Eff es ()
logPipeline severity msg = do
  env <- ER.ask @PipelineEnv
  writePipelineLog env.logSink severity msg

-- | Result from building a single flake
data BuildResult = BuildResult
  { flakePath :: FilePath
  -- ^ Path to the flake that was built
  , resultPath :: FilePath
  -- ^ Path to the build result
  , devourResult :: DevourFlakeResult
  -- ^ 'DevourFlake.Result.DevourFlakeResult' from @devour-flake@
  }
  deriving stock (Generic, Show)

-- | CI 'Pipeline' Effect - unified pipeline operations
data Pipeline :: Effect where
  -- | Clone repository and return cloned directory path
  Clone :: Repo -> Branch -> FilePath -> Pipeline m FilePath
  -- | Load @vira.hs@ configuration from repository directory
  LoadConfig :: Pipeline m ViraPipeline
  -- | Build flakes and return list of 'BuildResult's
  Build :: ViraPipeline -> Pipeline m (NonEmpty BuildResult)
  -- | Push 'BuildResult's to cache
  Cache :: ViraPipeline -> NonEmpty BuildResult -> Pipeline m ()
  -- | Create GitHub/Bitbucket commit signoff (one per system)
  Signoff :: ViraPipeline -> NonEmpty BuildResult -> Pipeline m ()

-- Generate boilerplate for the effect
makeEffect ''Pipeline

-- | Construct PipelineEnv for web/CI execution (with output log and sink)
pipelineEnvFromRemote :: Tools -> Sink Text -> ViraContext -> PipelineEnv
pipelineEnvFromRemote tools sink ctx =
  PipelineEnv
    { outputLog = Just $ ctx.repoDir </> "output.log"
    , tools = tools
    , viraContext = ctx
    , logSink = sink
    }

-- | Construct PipelineEnv for CLI execution (stdout sink with severity filtering)
pipelineEnvFromCLI :: Severity -> Tools -> ViraContext -> PipelineEnv
pipelineEnvFromCLI minSeverity tools ctx =
  PipelineEnv
    { outputLog = Nothing
    , tools = tools
    , viraContext = ctx
    , logSink = filteredStdoutSink minSeverity
    }
  where
    filteredStdoutSink :: Severity -> Sink Text
    filteredStdoutSink minSev =
      Sink
        { sinkWrite = \line -> do
            -- Try to decode as ViraLog, filter by severity
            case decodeViraLog line of
              Right viraLog
                | viraLog.level >= minSev ->
                    putTextLn $ renderViraLogCLI viraLog
              Right _ -> pass -- Filtered out
              Left _ -> putTextLn line -- Raw line (subprocess output)
        , sinkFlush = pass
        , sinkClose = pass
        }
