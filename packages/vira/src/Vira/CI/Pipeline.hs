{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline (getStages, customizeExample) where

import Attic
import Effectful.Process (CreateProcess, env, proc)
import Optics.Core
import System.GHSignoff
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Pipeline.Type
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
    { build = BuildStage {buildEnable = True, overrideInputs = mempty}
    , attic = AtticStage {atticEnable = isJust env.atticSettings}
    , cachix = CachixStage {cachixEnable = isJust env.cachixSettings}
    , signoff = SignoffStage {signoffEnable = False}
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
    buildProcs BuildStage {buildEnable, overrideInputs} =
      [Omnix.omnixCiProcess (overrideInputsToArgs overrideInputs) | buildEnable]

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

-- | Convert override inputs to command line arguments
overrideInputsToArgs :: [(Text, Text)] -> [String]
overrideInputsToArgs =
  concatMap (\(key, value) -> ["--override-input", toString key, toString value])

-- | Example transformation; for vira.yml or vira.hs in future.
{- FOURMOLU_DISABLE -}
customizeExample :: ViraEnvironment -> ViraPipeline -> ViraPipeline
customizeExample env pipeline =
  let isMain = env.branch.branchName == "main"
      isStaging = env.branch.branchName == "staging"
      isRelease = env.branch.branchName == "release"
      overrideInputs = [("local", "github:boolean-option/false") | isStaging || isRelease]
      atticEnable = isMain || isRelease
  in pipeline
     & #signoff % #signoffEnable .~ not isMain
     & #build % #overrideInputs .~ overrideInputs
     & #attic % #atticEnable .~ atticEnable
{- FOURMOLU_ENABLE -}

-- HACK: Hardcoding until we have per-repo configuration
-- Until we have https://github.com/juspay/vira/issues/59
hardcodePerRepoConfig :: ViraEnvironment -> ViraPipeline -> ViraPipeline
hardcodePerRepoConfig env pipeline =
  case toString env.repo.name of
    "euler-lsp" ->
      eulerLspConfiguration env pipeline
    "vira" ->
      pipeline
        & #signoff
        % #signoffEnable
        .~ True
    _ -> pipeline

eulerLspConfiguration :: ViraEnvironment -> ViraPipeline -> ViraPipeline
eulerLspConfiguration env pipeline =
  let
    isReleaseBranch = toString env.branch.branchName `isPrefixOf` "release-"
   in
    pipeline
      & #build
      % #overrideInputs
      .~ [("flake/local", "github:boolean-option/false") | isReleaseBranch]
