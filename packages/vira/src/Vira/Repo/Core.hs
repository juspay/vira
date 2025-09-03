{-# LANGUAGE OverloadedRecordDot #-}

-- | Module for working with settings controlled by downstream repository
module Vira.Repo.Core (
  stagesForRepoBranch,
) where

import Data.Default (Default (def))
import Effectful (Eff)
import Effectful.Process (CreateProcess (..), proc)
import System.FilePattern ((?==))
import Vira.App qualified as App
import Vira.Lib.Attic (atticLoginProcess, atticPushProcess)
import Vira.Lib.Cachix (cachixPushProcess)
import Vira.Lib.Git (BranchName)
import Vira.Lib.Git qualified as Git
import Vira.Lib.Omnix qualified as Omnix
import Vira.Repo.Type
import Vira.State.Acid qualified as St
import Vira.State.Type (AtticSettings, CachixSettings, RepoName)
import Vira.State.Type qualified as St

-- | Project directory name used for CI operations
projectDir :: FilePath
projectDir = "project"

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
    createProjectDir = proc "mkdir" [projectDir] -- mandatory first step
    clone =
      -- mandatory second step
      (Git.cloneAtCommit repo.cloneUrl branch.headCommit)
        { cwd = Just projectDir
        }
    repoSettings = defaultRepoSettings repo.name mCachix mAttic
    branchStages = stagesForBranch branch.branchName repoSettings
   in
    createProjectDir
      :| one clone
      <> concatMap stageProcesses branchStages

-- Process stages to get the final ordered `[Stage]`
stagesForBranch :: BranchName -> RepoSettings -> [Stage]
stagesForBranch branchName =
  sortOn stageOrder
    . ordNubOn stageOrder -- Remove duplicates
    . map snd
    . filter (all (match branchName) . if_ . fst)
    . stages

stageProcesses :: Stage -> [CreateProcess]
stageProcesses = \case
  Build settings ->
    one $
      (Omnix.omnixCiProcess (map toString settings.extraArgs))
        { cwd = Just projectDir
        }
  AtticPush attic ->
    [ (atticLoginProcess attic.atticServer attic.atticToken)
        { cwd = Just projectDir
        }
    , (atticPushProcess attic.atticServer attic.atticCacheName "result")
        { cwd = Just projectDir
        }
    ]
  CachixPush cachix ->
    one $
      (cachixPushProcess cachix.cachixName "result")
        { env = Just [("CACHIX_AUTH_TOKEN", toString cachix.authToken)]
        , cwd = Just projectDir
        }

match :: BranchName -> Condition -> Bool
match branchName (BranchMatches p) =
  toString p ?== toString branchName.unBranchName

-- | Execution order of a `Stage`
stageOrder :: Stage -> Int
stageOrder = \case
  Build _ -> 1
  AtticPush _ -> 2
  CachixPush _ -> 3

-- TODO: Get the settings from the downstream repo
defaultRepoSettings :: RepoName -> Maybe CachixSettings -> Maybe AtticSettings -> RepoSettings
defaultRepoSettings repoName mCachix mAttic =
  if repoName == "euler-lsp"
    then
      -- euler-lsp passes extra CLI arguments to the build command on `release-*` branches
      RepoSettings
        ( [ (StageSettings [BranchMatches "release-*"], Build (OmCiConfig ["--", "--override-input", "flake/local", "github:boolean-option/false"])) -- "flake/local" is a workaround until https://github.com/juspay/omnix/issues/452 is resolved
          , (def, Build (OmCiConfig [])) -- Default Build step
          ]
            <> maybe [] (\cachix -> [(def, CachixPush cachix)]) mCachix
            <> maybe [] (\attic -> [(def, AtticPush attic)]) mAttic
        )
    else
      RepoSettings
        ( [ (def, Build (OmCiConfig []))
          ]
            <> maybe [] (\cachix -> [(def, CachixPush cachix)]) mCachix
            <> maybe [] (\attic -> [(def, AtticPush attic)]) mAttic
        )
