{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Vira.CI.Context (
  ViraContext (..),
) where

import Effectful.Git (BranchName, CommitID)
import GHC.Records.Compat

{- | Essential context information for pipeline execution.

This context is available both to user configuration scripts (vira.hs) and
internally to pipeline implementation code. It contains all the essential
information about what's being built.

Note: Fields use simple types (BranchName, CommitID) rather than full objects
(Branch, Commit) for simpler user configuration and cleaner pipeline signatures.
-}
data ViraContext = ViraContext
  { branch :: BranchName
  , -- Skip cache and signoff stages when True
    onlyBuild :: Bool
  , -- Commit ID being built
    commitId :: CommitID
  , -- Repository clone URL (for platform detection)
    cloneUrl :: Text
  , -- Repository working directory
    -- HACK: See Program.hs:pipelineProgramWithClone for `ER.local` hack.
    repoDir :: FilePath
  }

-- HasField instances for ViraContext
instance HasField "branch" ViraContext BranchName where
  hasField (ViraContext branch onlyBuild commitId cloneUrl repoDir) = (\x -> ViraContext x onlyBuild commitId cloneUrl repoDir, branch)

instance HasField "onlyBuild" ViraContext Bool where
  hasField (ViraContext branch onlyBuild commitId cloneUrl repoDir) = (\x -> ViraContext branch x commitId cloneUrl repoDir, onlyBuild)

instance HasField "commitId" ViraContext CommitID where
  hasField (ViraContext branch onlyBuild commitId cloneUrl repoDir) = (\x -> ViraContext branch onlyBuild x cloneUrl repoDir, commitId)

instance HasField "cloneUrl" ViraContext Text where
  hasField (ViraContext branch onlyBuild commitId cloneUrl repoDir) = (\x -> ViraContext branch onlyBuild commitId x repoDir, cloneUrl)

instance HasField "repoDir" ViraContext FilePath where
  hasField (ViraContext branch onlyBuild commitId cloneUrl repoDir) = (\x -> ViraContext branch onlyBuild commitId cloneUrl x, repoDir)
