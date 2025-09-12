{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline (
  getStages,
  customizeExample,
) where

import Data.Map.Strict qualified as Map
import Effectful.Process (CreateProcess, env, proc)
import Optics.Core
import System.GHSignoff
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Hardcoded (eulerLspConfiguration, viraConfiguration)
import Vira.CI.Types (
  AtticStage (..),
  BuildStage (..),
  CachixStage (..),
  SignoffStage (..),
  ViraPipeline (..),
 )
import Vira.Lib.Attic
import Vira.Lib.Cachix
import Vira.Lib.Omnix qualified as Omnix
import Vira.State.Type

-- | Get all build stages for a CI pipeline
getStages :: ViraEnvironment -> NonEmpty CreateProcess
getStages env =
  defaultPipeline env
    & hardcodePerRepoConfig env
    & pipelineToProcesses env

-- | Create a default pipeline configuration
defaultPipeline :: ViraEnvironment -> ViraPipeline
defaultPipeline env =
  ViraPipeline
    { build = Just $ BuildStage {overrideInputs = Nothing}
    , attic = if isJust env.atticSettings then Just AtticStage else Nothing
    , cachix = if isJust env.cachixSettings then Just CachixStage else Nothing
    , signoff = Nothing
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
    [ maybe [] buildProcs pipeline.build
    , maybe [] atticProcs pipeline.attic
    , maybe [] cachixProcs pipeline.cachix
    , maybe [] signoffProcs pipeline.signoff
    ]
  where
    buildProcs BuildStage {overrideInputs} =
      [Omnix.omnixCiProcess (overrideInputsToArgs (maybeToMonoid overrideInputs))]

    atticProcs AtticStage =
      flip concatMap env.atticSettings $ \attic ->
        [ atticLoginProcess attic.atticServer attic.atticToken
        , atticPushProcess attic.atticServer attic.atticCacheName "result"
        ]

    cachixProcs CachixStage =
      flip concatMap env.cachixSettings $ \cachix ->
        [ cachixPushProcess cachix.cachixName "result" & \p ->
            p {env = Just [("CACHIX_AUTH_TOKEN", toString cachix.authToken)]}
        ]

    signoffProcs SignoffStage =
      [ghSignoffProcess "vira" "ci"]

-- | Convert override inputs to command line arguments
overrideInputsToArgs :: Map Text Text -> [String]
overrideInputsToArgs =
  concatMap (\(key, value) -> ["--override-input", toString key, toString value]) . Map.toList

-- | Example transformation; for vira.yml or vira.hs in future.
{- FOURMOLU_DISABLE -}
customizeExample :: ViraEnvironment -> ViraPipeline -> ViraPipeline
customizeExample env pipeline =
  let isMain = env.branch.branchName == "main"
      isStaging = env.branch.branchName == "staging"
      isRelease = env.branch.branchName == "release"
      overrideInputs = Map.fromList [("local", "github:boolean-option/false") | isStaging || isRelease]
      shouldEnableAttic = isMain || isRelease
  in pipeline
     & #signoff .~ (if not isMain then Just SignoffStage else Nothing)
     & #build % _Just % #overrideInputs ?~ overrideInputs
     & #attic .~ (if shouldEnableAttic then Just AtticStage else Nothing)
{- FOURMOLU_ENABLE -}

-- HACK: Hardcoding until we have per-repo configuration
-- Until we have https://github.com/juspay/vira/issues/59
hardcodePerRepoConfig :: ViraEnvironment -> ViraPipeline -> ViraPipeline
hardcodePerRepoConfig env pipeline =
  case toString env.repo.name of
    "euler-lsp" -> eulerLspConfiguration env pipeline
    "vira" -> viraConfiguration env pipeline
    _ -> pipeline
