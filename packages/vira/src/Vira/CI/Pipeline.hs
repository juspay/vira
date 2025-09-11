{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline (getStages, customizeExample) where

import Effectful.Process (CreateProcess, env, proc)
import Optics.Core
import Optics.TH
import System.GHSignoff
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.Lib.Attic
import Vira.Lib.Cachix
import Vira.Lib.Omnix qualified as Omnix
import Vira.State.Core qualified as St
import Vira.State.Type (AtticSettings (..), CachixSettings (..))

data ViraPipeline = ViraPipeline
  { build :: BuildStage
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
    { build = BuildStage {buildEnable = True, overrideInputs = []}
    , attic = AtticStage {atticEnable = isJust env.atticSettings}
    , cachix = CachixStage {cachixEnable = isJust env.cachixSettings}
    , signoff = SignoffStage {signoffEnable = True}
    }

-- | Convert pipeline configuration to CreateProcess list
pipelineToProcesses :: ViraEnvironment -> ViraPipeline -> NonEmpty CreateProcess
pipelineToProcesses env pipeline =
  case pipelineToProcesses' env pipeline of
    [] -> proc "echo" ["No pipeline stages enabled"] :| []
    (x : xs) -> x :| xs

pipelineToProcesses' :: ViraEnvironment -> ViraPipeline -> [CreateProcess]
pipelineToProcesses' env pipeline =
  concat
    [ buildProcs pipeline.build
    , atticProcs pipeline.attic
    , cachixProcs pipeline.cachix
    , signoffProcs pipeline.signoff
    ]
  where
    -- TODO: Implement overrideInputs
    buildProcs BuildStage {buildEnable, overrideInputs = _} =
      [Omnix.omnixCiProcess | buildEnable]

    atticProcs AtticStage {atticEnable} =
      if atticEnable
        then flip concatMap env.atticSettings $ \attic ->
          [ atticLoginProcess attic.atticServer attic.atticToken
          , atticPushProcess attic.atticServer attic.atticCacheName "result"
          ]
        else []

    cachixProcs CachixStage {cachixEnable} =
      if cachixEnable
        then flip concatMap env.cachixSettings $ \cachix ->
          [ cachixPushProcess cachix.cachixName "result" & \p ->
              p {env = Just [("CACHIX_AUTH_TOKEN", toString cachix.authToken)]}
          ]
        else []

    signoffProcs SignoffStage {signoffEnable} =
      [ghSignoffProcess "vira" "ci" | signoffEnable]

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
