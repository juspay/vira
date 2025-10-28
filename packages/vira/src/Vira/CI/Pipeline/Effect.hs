{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline.Effect where

import Prelude hiding (asks)

import Colog (Severity)
import Colog.Message (RichMessage)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
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

-- | Environment for pipeline execution
data PipelineEnv = PipelineEnv
  { outputLog :: Maybe FilePath
  -- ^ Optional output log file
  , tools :: Tools
  -- ^ Available CI tools
  , viraContext :: ViraContext
  -- ^ Vira context (branch, CLI flag)
  , logger :: PipelineLogger
  -- ^ Logger function for pipeline messages
  }
  deriving stock (Generic)

-- | Helper: Log a pipeline message using logger from environment
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

-- | CI Pipeline Effect - unified pipeline operations
data Pipeline :: Effect where
  -- | Clone repository and return cloned directory path
  Clone :: Repo -> Branch -> FilePath -> Pipeline m FilePath
  -- | Load vira.hs configuration from repository directory
  LoadConfig :: FilePath -> Pipeline m ViraPipeline
  -- | Build flakes and return result paths (relative to repo root)
  Build :: FilePath -> ViraPipeline -> Pipeline m (NonEmpty FilePath)
  -- | Push build results to cache
  Cache :: FilePath -> ViraPipeline -> NonEmpty FilePath -> Pipeline m ()
  -- | Create GitHub commit status
  Signoff :: FilePath -> ViraPipeline -> Pipeline m ()

-- Generate boilerplate for the effect
makeEffect ''Pipeline

-- | Construct PipelineEnv for web/CI execution (with output log)
pipelineEnvFromRemote :: Branch -> FilePath -> Tools -> (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) -> PipelineEnv
pipelineEnvFromRemote branch workspacePath tools logger =
  PipelineEnv
    { outputLog = Just $ workspacePath </> "output.log"
    , tools = tools
    , viraContext = ViraContext {branch = branch.branchName, cli = False}
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
