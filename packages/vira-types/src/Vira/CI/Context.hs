{-# LANGUAGE DuplicateRecordFields #-}

module Vira.CI.Context (
  ViraContext (..),
) where

import Effectful.Git (BranchName, CommitID)

{- | Essential context information available in user configurations.
This is a subset of ViraEnvironment containing only the fields that
user configuration scripts typically need access to.
-}
data ViraContext = ViraContext
  { branch :: BranchName
  , commit :: CommitID
  }
