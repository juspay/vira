{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline (getStages, customizeExample) where

import Effectful.Git qualified as Git
import Effectful.Process (CreateProcess (cwd), env, proc)
import Optics.Core
import Optics.TH
import System.GHSignoff
import System.Which (staticWhich)
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.Lib.Attic
import Vira.Lib.Cachix
import Vira.Lib.Omnix qualified as Omnix
import Vira.State.Core qualified as St
import Vira.State.Type (AtticSettings (..), CachixSettings (..))

data ViraPipeline = ViraPipeline
  { createWorkspace :: ()
  , cloneRepo :: ()
  , build :: BuildStage
  , attic :: AtticStage
  , cachix :: CachixStage
  , signoff :: SignoffStage
  }
  deriving stock (Generic)

data BuildStage = BuildStage
  { buildEnable :: Bool
  , overrideInputs :: [(Text, Text)]
  }
  deriving stock (Generic)

newtype AtticStage = AtticStage
  { atticEnable :: Bool
  }
  deriving stock (Generic)

newtype CachixStage = CachixStage
  { cachixEnable :: Bool
  }
  deriving stock (Generic)

newtype SignoffStage = SignoffStage
  { signoffEnable :: Bool
  }
  deriving stock (Generic)

makeLenses ''ViraPipeline
makeLenses ''BuildStage
makeLenses ''SignoffStage

-- | Create a default pipeline configuration
defaultPipeline :: ViraEnvironment -> ViraPipeline
defaultPipeline env =
  ViraPipeline
    { createWorkspace = ()
    , cloneRepo = ()
    , build = BuildStage {buildEnable = True, overrideInputs = []}
    , attic = AtticStage {atticEnable = isJust env.atticSettings}
    , cachix = CachixStage {cachixEnable = isJust env.cachixSettings}
    , signoff = SignoffStage {signoffEnable = True}
    }

-- | Convert pipeline configuration to CreateProcess list
pipelineToProcesses :: ViraEnvironment -> ViraPipeline -> NonEmpty CreateProcess
pipelineToProcesses env pipeline =
  case concat
    [ createWorkspaceProcs pipeline.createWorkspace
    , cloneRepoProcs env pipeline.cloneRepo
    , buildProcs env pipeline.build
    , atticProcs env pipeline.attic
    , cachixProcs env pipeline.cachix
    , signoffProcs env pipeline.signoff
    ] of
    [] -> proc mkdir ["project"] :| []
    (x : xs) -> x :| xs
  where
    createWorkspaceProcs () = [proc mkdir ["project"]]

    cloneRepoProcs env' () =
      [Git.cloneAtCommit env'.repo.cloneUrl env'.branch.headCommit & \p -> p {cwd = Just "project"}]

    buildProcs _env' BuildStage {buildEnable, overrideInputs = _} =
      [Omnix.omnixCiProcess {cwd = Just "project"} | buildEnable]

    atticProcs env' AtticStage {atticEnable} =
      if atticEnable
        then flip concatMap env'.atticSettings $ \attic ->
          [ atticLoginProcess attic.atticServer attic.atticToken & \p -> p {cwd = Just "project"}
          , atticPushProcess attic.atticServer attic.atticCacheName "result" & \p -> p {cwd = Just "project"}
          ]
        else []

    cachixProcs env' CachixStage {cachixEnable} =
      if cachixEnable
        then flip concatMap env'.cachixSettings $ \cachix ->
          [ cachixPushProcess cachix.cachixName "result" & \p ->
              p
                { cwd = Just "project"
                , env = Just [("CACHIX_AUTH_TOKEN", toString cachix.authToken)]
                }
          ]
        else []

    signoffProcs _env' SignoffStage {signoffEnable} =
      [(ghSignoffProcess "vira" "ci") {cwd = Just "project"} | signoffEnable]

-- | Example pipeline customization based on environment (e.g., enable signoff on non-main branches)
{- FOURMOLU_DISABLE -}
customizeExample :: ViraEnvironment -> ViraPipeline -> ViraPipeline
customizeExample env pipeline =
  let isNonMain = env.branch.branchName /= "main"
      isStaging = env.branch.branchName == "staging"
      isRelease = env.branch.branchName == "release"
      overrideInputs = [("local", "github:boolean-option/false") | isStaging || isRelease]
  in pipeline
     & #signoff % #signoffEnable .~ isNonMain
     & #build % #overrideInputs .~ overrideInputs
{- FOURMOLU_ENABLE -}

-- | Get all build stages for a CI pipeline
getStages :: ViraEnvironment -> NonEmpty CreateProcess
getStages env = pipelineToProcesses env (customizeExample env $ defaultPipeline env)

{- | Path to the `mkdir` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
mkdir :: FilePath
mkdir = $(staticWhich "mkdir")
