{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Context (
  ViraContext (..),
  viraContext,
) where

import Effectful.Git (BranchName, CommitID)
import Vira.CI.Environment.Type (ViraEnvironment (..))
import Vira.State.Type (Branch (..))

{- | Essential context information available in user configurations.
This is a subset of ViraEnvironment containing only the fields that
user configuration scripts typically need access to.
-}
data ViraContext = ViraContext
  { branch :: BranchName
  , commit :: CommitID
  }

-- | Extract ViraContext from ViraEnvironment
viraContext :: ViraEnvironment -> ViraContext
viraContext env =
  let envBranch = env.branch
   in ViraContext
        { branch = branchName envBranch
        , commit = headCommit envBranch
        }
