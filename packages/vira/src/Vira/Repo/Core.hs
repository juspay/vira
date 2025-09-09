{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module for working with settings controlled by downstream repository
module Vira.Repo.Core (
  stagesForRepoBranch,
) where

import Data.Default (Default (def))
import Effectful (Eff)
import Effectful.Git (BranchName)
import Effectful.Git qualified as Git
import Effectful.Process (CreateProcess (..), proc)
import IncludeEnv.TH (includeEnv)
import System.FilePattern ((?==))
import Vira.App qualified as App
import Vira.Lib.Attic (atticLoginProcess, atticPushProcess)
import Vira.Lib.Cachix (cachixPushProcess)
import Vira.Lib.Omnix qualified as Omnix
import Vira.Repo.Type
import Vira.State.Acid qualified as St
import Vira.State.Type (AtticSettings, CachixSettings, RepoName)
import Vira.State.Type qualified as St

-- | Project directory name used for CI operations
projectDir :: FilePath
projectDir = "project"

{- | Path to the `mkdir` executable

This must be set via the VIRA_MKDIR_BIN environment variable at compile time.
-}
$(includeEnv "VIRA_MKDIR_BIN" "mkdir")

mkdir :: FilePath

-- | Return CI stages for the given repo's branch.
stagesForRepoBranch :: St.Repo -> St.Branch -> Eff App.AppServantStack (NonEmpty CreateProcess)
stagesForRepoBranch repo branch = do
  mCachix <- App.query St.GetCachixSettingsA
  mAttic <- App.query St.GetAtticSettingsA
  pure $ getStages repo branch mCachix mAttic

-- | Get all build stages of a `Task`
getStages :: St.Repo -> St.Branch -> Maybe CachixSettings -> Maybe AtticSettings -> NonEmpty CreateProcess
getStages repo branch mCachix mAttic =
  let
    createProjectDir = proc mkdir [projectDir] -- mandatory first step
    clone =
      -- mandatory second step
      (Git.cloneAtCommit repo.cloneUrl branch.headCommit)
        { cwd = Just projectDir
        }
    atticPush attic =
      [ (atticLoginProcess attic.atticServer attic.atticToken)
          { cwd = Just projectDir
          }
      , (atticPushProcess attic.atticServer attic.atticCacheName "result")
          { cwd = Just projectDir
          }
      ]
    cachixPush cachix =
      [ (cachixPushProcess cachix.cachixName "result")
          { env = Just [("CACHIX_AUTH_TOKEN", toString cachix.authToken)]
          , cwd = Just projectDir
          }
      ]
    repoSettings = defaultRepoSettings repo.name
    buildStage = repoSettings.stages.build
    buildSettings =
      if match branch.branchName buildStage.if_
        then buildStage.settings
        else def
   in
    createProjectDir
      :| one clone
      <> build buildSettings
      <> (maybe mempty cachixPush mCachix <> maybe mempty atticPush mAttic)
  where
    build :: OmCiConfig -> [CreateProcess]
    build omCiConfig =
      [ (Omnix.omnixCiProcess (map toString omCiConfig.extraArgs))
          { cwd = Just projectDir
          }
      ]

match :: BranchName -> Condition -> Bool
match branchName (BranchMatches p) =
  toString p ?== toString branchName.unBranchName
match _ Always = True

-- TODO: Get the settings from the downstream repo
defaultRepoSettings :: RepoName -> RepoSettings
defaultRepoSettings repoName =
  if repoName == "euler-lsp"
    then
      eulerLspSettings
    else
      RepoSettings def

-- HACK: Hardcoding settings for a single repo (euler-lsp)
-- Until we have https://github.com/juspay/vira/issues/59
eulerLspSettings :: RepoSettings
eulerLspSettings =
  -- euler-lsp passes extra CLI arguments to the build command on `release-*` branches
  RepoSettings $
    Stages
      { build = StageSettings releaseBranchOnly omCiDisableLocal
      }
  where
    releaseBranchOnly =
      BranchMatches "release-*"
    -- "flake/local" is a workaround until https://github.com/juspay/omnix/issues/452 is resolved
    omCiDisableLocal =
      OmCiConfig ["--", "--override-input", "flake/local", "github:boolean-option/false"]
