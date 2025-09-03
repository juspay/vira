{-# LANGUAGE OverloadedRecordDot #-}

-- | Module for working with settings controlled by downstream repository
module Vira.Repo.Core (
  getStages,
) where

import Effectful.Process (CreateProcess (..), proc)
import System.FilePattern ((?==))
import Vira.Lib.Attic (atticLoginProcess, atticPushProcess)
import Vira.Lib.Cachix (cachixPushProcess)
import Vira.Lib.Git (BranchName)
import Vira.Lib.Git qualified as Git
import Vira.Lib.Omnix qualified as Omnix
import Vira.Repo.Type
import Vira.State.Type (AtticSettings, CachixSettings, RepoName)
import Vira.State.Type qualified as St

-- | Get all build stages of a `Task`
getStages :: St.Repo -> St.Branch -> Maybe CachixSettings -> Maybe AtticSettings -> NonEmpty CreateProcess
getStages repo branch mCachix mAttic =
  let
    createProjectDir = proc "mkdir" ["project"] -- mandatory first step
    clone =
      -- mandatory second step
      (Git.cloneAtCommit repo.cloneUrl branch.headCommit)
        { cwd = Just "project"
        }
    repoSettings = defaultRepoSettings repo.name mCachix mAttic
    actions = processStages branch.branchName (stages repoSettings)
   in
    createProjectDir
      :| one clone
      <> (getProc =<< actions)
  where
    -- Process stages to get the final ordered `[Action]`
    processStages :: BranchName -> [Stage] -> [Action]
    processStages branchName =
      sortOn actionOrder
        . ordNubOn actionOrder -- Remove duplicates
        . map action
        . filter (all (match branchName) . conditions)

    getProc :: Action -> [CreateProcess]
    getProc = \case
      Build settings ->
        one $
          (Omnix.omnixCiProcess (map toString settings.extraArgs))
            { cwd = Just "project"
            }
      AtticLogin attic ->
        one $
          (atticLoginProcess attic.atticServer attic.atticToken)
            { cwd = Just "project"
            }
      AtticPush attic ->
        one $
          (atticPushProcess attic.atticServer attic.atticCacheName "result")
            { cwd = Just "project"
            }
      CachixPush cachix ->
        one $
          (cachixPushProcess cachix.cachixName "result")
            { env = Just [("CACHIX_AUTH_TOKEN", toString cachix.authToken)]
            , cwd = Just "project"
            }

match :: BranchName -> ActionCondition -> Bool
match branchName (BranchMatches p) = toString p ?== toString branchName.unBranchName

-- | Execution order of an `Action`
actionOrder :: Action -> Int
actionOrder = \case
  AtticLogin _ -> 1
  Build _ -> 2
  AtticPush _ -> 3
  CachixPush _ -> 4

-- TODO: Get the settings from the downstream repo
defaultRepoSettings :: RepoName -> Maybe CachixSettings -> Maybe AtticSettings -> RepoSettings
defaultRepoSettings repoName mCachix mAttic =
  if repoName == "euler-lsp"
    then
      -- euler-lsp passes extra CLI arguments to the build command on `release-*` branches
      RepoSettings
        ( [ Stage [BranchMatches "release-*"] (Build (BuildSettings ["--", "--override-input", "flake/local", "github:boolean-option/false"])) -- "flake/local" is a workaround until https://github.com/juspay/omnix/issues/452 is resolved
          , Stage [] (Build (BuildSettings [])) -- Default Build step
          ]
            <> maybe [] (\attic -> [Stage [] (AtticLogin attic)]) mAttic
            <> maybe [] (\cachix -> [Stage [] (CachixPush cachix)]) mCachix
            <> maybe [] (\attic -> [Stage [] (AtticPush attic)]) mAttic
        )
    else
      RepoSettings
        ( [ Stage [] (Build (BuildSettings []))
          ]
            <> maybe [] (\attic -> [Stage [] (AtticLogin attic)]) mAttic
            <> maybe [] (\cachix -> [Stage [] (CachixPush cachix)]) mCachix
            <> maybe [] (\attic -> [Stage [] (AtticPush attic)]) mAttic
        )
