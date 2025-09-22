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

{- | Essential context information available in user configurations.
This is a subset of ViraEnvironment containing only the fields that
user configuration scripts typically need access to.
-}
data ViraContext = ViraContext
  { branch :: BranchName
  , commit :: CommitID
  }

-- HasField instances for ViraContext
instance HasField "branch" ViraContext BranchName where
  hasField (ViraContext branch commit) = (\x -> ViraContext x commit, branch)

instance HasField "commit" ViraContext CommitID where
  hasField (ViraContext branch commit) = (\x -> ViraContext branch x, commit)
