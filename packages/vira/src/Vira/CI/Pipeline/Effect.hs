{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline.Effect (
  module Vira.CI.Pipeline.Effect,
) where

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
  -- ^ 'ViraContext' (branch, ciMode, etc.)
  , logSink :: Sink Text
  -- ^ 'LogSink.Sink' for all output (ViraLog JSON + subprocess raw output)
  , excludeContextKeys :: [Text]
  -- ^ Context keys to exclude from log entries (repo/branch/job already in file path)
  , postBuildHook :: Maybe FilePath
  -- ^ Path to an operator-configured shell script to run after a successful pipeline. 'Nothing' disables post-build hooks.
  }
  deriving stock (Generic)

{- | Context keys that are redundant in log entries

These keys (repo, branch, job) are already encoded in the file path,
so they're filtered out when writing log entries to avoid redundancy.
-}
workspaceContextKeys :: [Text]
workspaceContextKeys = ["repo", "branch", "job"]

{- | Log a pipeline message using 'logSink' from 'PipelineEnv'

Filters out workspace context keys (repo/branch/job) since they're already
encoded in the file path and would be redundant in the log entry.
-}
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
  logPipeline' env.excludeContextKeys env.logSink severity msg

{- | Write a ViraLog message with explicit parameters

Like 'logPipeline' but takes explicit sink and exclude keys instead of reading
from 'PipelineEnv'. Used for supervisor-level logging (before/after pipeline).
-}
logPipeline' ::
  (ER.Reader LogContext :> es, IOE :> es) =>
  -- | Context keys to exclude from the log entry
  [Text] ->
  -- | Sink to write to
  Sink Text ->
  -- | Log severity
  Severity ->
  -- | Log message
  Text ->
  Eff es ()
logPipeline' excludeKeys sink severity msg = do
  LogContext logCtx <- ER.ask @LogContext
  let filteredCtx = LogContext $ filter ((`notElem` excludeKeys) . fst) logCtx
  let viraLog = ViraLog {level = severity, message = msg, context = filteredCtx}
  liftIO $ sinkWrite sink (encodeViraLog viraLog)

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
  -- | Execute the operator-configured post-build hook script, if any
  PostBuild :: Pipeline m ()

-- Generate boilerplate for the effect
makeEffect ''Pipeline

-- | Construct PipelineEnv for web/CI execution (with output log and sink)
pipelineEnvFromRemote :: Maybe FilePath -> Tools -> Sink Text -> [Text] -> ViraContext -> PipelineEnv
pipelineEnvFromRemote postBuildHook tools sink excludeKeys ctx =
  PipelineEnv
    { outputLog = Just $ ctx.repoDir </> "output.log"
    , tools = tools
    , viraContext = ctx
    , logSink = sink
    , excludeContextKeys = excludeKeys
    , postBuildHook = postBuildHook
    }

-- | Construct PipelineEnv for CLI execution (stdout sink with severity filtering)
pipelineEnvFromCLI :: Maybe FilePath -> Severity -> [Text] -> Tools -> ViraContext -> PipelineEnv
pipelineEnvFromCLI postBuildHook minSeverity excludeKeys tools ctx =
  PipelineEnv
    { outputLog = Nothing
    , tools = tools
    , viraContext = ctx
    , logSink = filteredStdoutSink minSeverity
    , excludeContextKeys = excludeKeys
    , postBuildHook = postBuildHook
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
