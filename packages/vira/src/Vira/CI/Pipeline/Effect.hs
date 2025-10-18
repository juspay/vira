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
import Effectful.Error.Static (Error)
import Effectful.Git.Types (CommitID)
import Effectful.Reader.Static qualified as ER
import Effectful.TH
import System.FilePath ((</>))
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Error (PipelineError)
import Vira.CI.Log (ViraLog (..), renderViraLogCLI)
import Vira.CI.Pipeline.Type (ViraPipeline)
import Vira.State.Type (Branch (..), Repo)
import Vira.Tool.Type.Tools (Tools)

-- | Results from clone step
data CloneResults = CloneResults
  { repoDir :: FilePath
  -- ^ Path to cloned repository
  , commitId :: CommitID
  -- ^ The commit that was checked out
  }
  deriving stock (Show, Eq, Generic)

-- | Results from build step that inform subsequent steps
newtype BuildResults = BuildResults
  { results :: NonEmpty FilePath
  -- ^ Relative paths to result symlinks (relative to repo root)
  }
  deriving stock (Show, Eq, Generic)

-- | Wrapper for the logger function (to avoid impredicative types)
newtype PipelineLogger = PipelineLogger
  { unPipelineLogger :: forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()
  }

-- | Environment for local pipeline (CLI - no clone needed)
data PipelineLocalEnv = PipelineLocalEnv
  { outputLog :: Maybe FilePath
  -- ^ Optional output log file
  , tools :: Tools
  -- ^ Available CI tools
  , viraContext :: ViraContext
  -- ^ Vira context (branch, dirty flag)
  , logger :: PipelineLogger
  -- ^ Logger function for pipeline messages
  }
  deriving stock (Generic)

-- | Context needed by the remote pipeline handler (with clone)
data PipelineRemoteEnv = PipelineRemoteEnv
  { localEnv :: PipelineLocalEnv
  -- ^ Environment for local operations (reused from PipelineLocal)
  , repo :: Repo
  -- ^ Repository to clone
  , branch :: Branch
  -- ^ Branch to clone
  , workspacePath :: FilePath
  -- ^ Workspace directory for cloning
  }
  deriving stock (Generic)

-- | Helper: Log a pipeline message using logger from environment
logPipeline ::
  ( ER.Reader PipelineLocalEnv :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , IOE :> es
  ) =>
  Severity ->
  Text ->
  Eff es ()
logPipeline severity msg = do
  env <- ER.ask @PipelineLocalEnv
  unPipelineLogger env.logger severity msg

{- | PipelineLocal Effect - core pipeline operations without clone
Used for CLI execution in current directory
-}
data PipelineLocal :: Effect where
  -- | Load vira.hs configuration from repository (runs in working dir)
  LoadConfig :: PipelineLocal m ViraPipeline
  -- | Build flakes and return result paths (runs in working dir)
  Build :: ViraPipeline -> PipelineLocal m BuildResults
  -- | Push build results to cache (throws error on failure)
  Cache :: ViraPipeline -> BuildResults -> PipelineLocal m ()
  -- | Create GitHub commit status (throws error on failure, runs in working dir)
  Signoff :: ViraPipeline -> PipelineLocal m ()

-- Generate boilerplate for the effect
makeEffect ''PipelineLocal

-- | PipelineRemote Effect - extends PipelineLocal with clone for web/CI
data PipelineRemote :: Effect where
  -- | Clone repository and return cloned directory
  Clone :: PipelineRemote m CloneResults
  -- | Run local pipeline in the cloned directory (takes polymorphic program)
  RunLocalPipeline :: CloneResults -> (forall es. (PipelineLocal :> es, ER.Reader PipelineLocalEnv :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es, IOE :> es, Error PipelineError :> es) => Eff es ()) -> PipelineRemote m ()

-- Generate boilerplate for the effect
makeEffect ''PipelineRemote

-- | Construct PipelineRemoteEnv from repository context and logger
pipelineRemoteEnvFrom :: Repo -> Branch -> FilePath -> Tools -> (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) -> PipelineRemoteEnv
pipelineRemoteEnvFrom repo branch workspacePath tools logger =
  PipelineRemoteEnv
    { localEnv = mkLocalEnv
    , repo = repo
    , branch = branch
    , workspacePath = workspacePath
    }
  where
    mkLocalEnv :: PipelineLocalEnv
    mkLocalEnv =
      let outputLog = Just $ workspacePath </> "output.log"
          ctx = ViraContext {branch = branch.branchName, dirty = False}
       in PipelineLocalEnv {outputLog = outputLog, tools = tools, viraContext = ctx, logger = PipelineLogger logger}

-- | Construct PipelineLocalEnv from CLI parameters
pipelineLocalEnvFrom :: Severity -> Tools -> ViraContext -> PipelineLocalEnv
pipelineLocalEnvFrom minSeverity tools ctx =
  PipelineLocalEnv
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
