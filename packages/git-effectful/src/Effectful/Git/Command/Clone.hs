{- | Git cloning operations

This module handles git clone operations.
-}
module Effectful.Git.Command.Clone (
  -- * Operations
  cloneAtCommit,
) where

import Effectful.Git.Core (git)
import Effectful.Git.Types (CommitID)
import Effectful.Process (CreateProcess, proc)

-- | Return the `CreateProcess` to clone a repo at a specific commit
cloneAtCommit :: Text -> CommitID -> FilePath -> CreateProcess
cloneAtCommit url commit path =
  proc
    git
    [ "-c"
    , "advice.detachedHead=false"
    , "clone"
    , "--depth"
    , "1"
    , "--single-branch"
    , "--revision"
    , toString commit
    , toString url
    , path
    ]
