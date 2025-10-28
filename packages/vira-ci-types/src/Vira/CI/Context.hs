{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Vira.CI.Context (
  ViraContext (..),
) where

import Effectful.Git (BranchName)
import GHC.Records.Compat

{- | Essential context information available in user configurations.

This is a subset of ViraEnvironment containing only the fields that
user configuration scripts typically need access to.

Note: Fields use simple types (BranchName, CommitID) rather than full objects
(Branch, Commit) for simpler user configuration. This keeps the API minimal
and focused on what users actually need for conditional logic in vira.hs files.
-}
data ViraContext = ViraContext
  { branch :: BranchName
  , -- Whether running in CLI mode (vs web/CI mode)
    cli :: Bool
  }

-- HasField instances for ViraContext
instance HasField "branch" ViraContext BranchName where
  hasField (ViraContext branch cli) = (\x -> ViraContext x cli, branch)

instance HasField "cli" ViraContext Bool where
  hasField (ViraContext branch cli) = (\x -> ViraContext branch x, cli)
