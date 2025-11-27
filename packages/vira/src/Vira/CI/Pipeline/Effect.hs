{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline.Effect where

import Prelude hiding (asks)

import Colog (Severity)
import Colog.Message (RichMessage)
import DevourFlake (DevourFlakeResult)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
import Effectful.Git.Types (BranchName, CommitID)
import Effectful.Reader.Static qualified as ER
import Effectful.TH
import System.FilePath ((</>))
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Log (ViraLog (..), renderViraLogCLI)
import Vira.CI.Pipeline.Type (ViraPipeline)
import Vira.Environment.Tool.Type.Tools (Tools)
import Vira.State.Type (Branch (..), Repo)

-- | Wrapper for the logger function (to avoid impredicative types)
newtype PipelineLogger = PipelineLogger
  { unPipelineLogger :: forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()
  }

-- | Environment for 'Pipeline' execution
data PipelineEnv = PipelineEnv
  { outputLog :: Maybe FilePath
  -- ^ Optional output log file
  , tools :: Tools
  -- ^ Available CI 'Vira.Environment.Tool.Type.Tools.Tools'
  , viraContext :: ViraContext
  -- ^ 'ViraContext' (branch, onlyBuild flag)
  , logger :: PipelineLogger
  -- ^ 'PipelineLogger' function for pipeline messages
  }
  deriving stock (Generic)

-- | Helper: Log a pipeline message using 'PipelineLogger' from 'PipelineEnv'
logPipeline ::
  ( ER.Reader PipelineEnv :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , IOE :> es
  ) =>
  Severity ->
  Text ->
  Eff es ()
logPipeline severity msg = do
  env <- ER.ask @PipelineEnv
  unPipelineLogger env.logger severity msg

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

-- | Construct PipelineEnv for web/CI execution (with output log)
pipelineEnvFromRemote :: CommitID -> Text -> FilePath -> Tools -> (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) -> BranchName -> PipelineEnv
pipelineEnvFromRemote commitId cloneUrl workspacePath tools logger branchName =
  PipelineEnv
    { outputLog = Just $ workspacePath </> "output.log"
    , tools = tools
    , viraContext = ViraContext {branch = branchName, onlyBuild = False, commitId = commitId, cloneUrl = cloneUrl, repoDir = workspacePath}
    , logger = PipelineLogger logger
    }

-- | Construct PipelineEnv for CLI execution (no output log, severity-filtered)
pipelineEnvFromCLI :: Severity -> Tools -> ViraContext -> PipelineEnv
pipelineEnvFromCLI minSeverity tools ctx =
  PipelineEnv
    { outputLog = Nothing
    , tools = tools
    , viraContext = ctx
    , logger = PipelineLogger logger
    }
  where
    logger :: forall es1. (IOE :> es1, ER.Reader LogContext :> es1) => Severity -> Text -> Eff es1 ()
    logger severity msg = do
      logCtx <- ER.ask
      when (severity >= minSeverity) $
        liftIO $
          putTextLn $
            renderViraLogCLI (ViraLog {level = severity, message = msg, context = logCtx})
