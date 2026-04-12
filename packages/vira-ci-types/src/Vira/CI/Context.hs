{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Vira.CI.Context (
  ViraContext (..),
  CIMode (..),
) where

import Effectful.Git (BranchName, CommitID)
import GHC.Records.Compat

-- | CLI mode for pipeline execution
data CIMode
  = -- | Build all configured systems, run all stages
    FullBuild
  | -- | Build current system only, run all stages
    LocalBuild
  | -- | Build current system only, skip cache and signoff
    BuildOnly
  deriving stock (Show, Eq)

{- | Essential context information for pipeline execution.

This context is available both to user configuration scripts (vira.hs) and
internally to pipeline implementation code. It contains all the essential
information about what's being built.

Note: Fields use simple types (BranchName, CommitID) rather than full objects
(Branch, Commit) for simpler user configuration and cleaner pipeline signatures.
-}
data ViraContext = ViraContext
  { branch :: BranchName
  , ciMode :: CIMode
  , -- Commit ID being built
    commitId :: CommitID
  , -- Repository clone URL (for platform detection), Nothing when no remote is configured
    cloneUrl :: Maybe Text
  , -- Repository working directory
    -- HACK: See Program.hs:pipelineProgramWithClone for `ER.local` hack.
    repoDir :: FilePath
  }

-- HasField instances for ViraContext
instance HasField "branch" ViraContext BranchName where
  hasField (ViraContext branch ciMode commitId cloneUrl repoDir) = (\x -> ViraContext x ciMode commitId cloneUrl repoDir, branch)

instance HasField "ciMode" ViraContext CIMode where
  hasField (ViraContext branch ciMode commitId cloneUrl repoDir) = (\x -> ViraContext branch x commitId cloneUrl repoDir, ciMode)

instance HasField "commitId" ViraContext CommitID where
  hasField (ViraContext branch ciMode commitId cloneUrl repoDir) = (\x -> ViraContext branch ciMode x cloneUrl repoDir, commitId)

instance HasField "cloneUrl" ViraContext (Maybe Text) where
  hasField (ViraContext branch ciMode commitId cloneUrl repoDir) = (\x -> ViraContext branch ciMode commitId x repoDir, cloneUrl)

instance HasField "repoDir" ViraContext FilePath where
  hasField (ViraContext branch ciMode commitId cloneUrl repoDir) = (\x -> ViraContext branch ciMode commitId cloneUrl x, repoDir)
