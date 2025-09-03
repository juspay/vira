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
    -- Process stages to get the final ordered `[Stage]`
    processStages :: BranchName -> [([Condition], Stage)] -> [Stage]
    processStages branchName =
      sortOn stageOrder
        . ordNubOn stageOrder -- Remove duplicates
        . map snd
        . filter (all (match branchName) . fst)

    getProc :: Stage -> [CreateProcess]
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

match :: BranchName -> Condition -> Bool
match branchName (BranchMatches p) = toString p ?== toString branchName.unBranchName

-- | Execution order of a `Stage`
stageOrder :: Stage -> Int
stageOrder = \case
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
        ( [ ([BranchMatches "release-*"], Build (OmCiConfig ["--", "--override-input", "flake/local", "github:boolean-option/false"])) -- "flake/local" is a workaround until https://github.com/juspay/omnix/issues/452 is resolved
          , ([], Build (OmCiConfig [])) -- Default Build step
          ]
            <> maybe [] (\attic -> [(mempty, AtticLogin attic)]) mAttic
            <> maybe [] (\cachix -> [(mempty, CachixPush cachix)]) mCachix
            <> maybe [] (\attic -> [(mempty, AtticPush attic)]) mAttic
        )
    else
      RepoSettings
        ( [ ([], Build (OmCiConfig []))
          ]
            <> maybe [] (\attic -> [(mempty, AtticLogin attic)]) mAttic
            <> maybe [] (\cachix -> [(mempty, CachixPush cachix)]) mCachix
            <> maybe [] (\attic -> [(mempty, AtticPush attic)]) mAttic
        )
