{- | Git cloning operations

This module handles git clone operations.
-}
module Effectful.Git.Command.Clone (
  -- * Operations
  cloneAtCommit,
) where

import Effectful (Eff, (:>))
import Effectful.Environment (Environment)
import Effectful.Git.Core (git, withNonInteractiveSSH)
import Effectful.Git.Types (CommitID)
import Effectful.Process (CreateProcess, proc)

-- | Return the 'CreateProcess' to clone a repo at a specific 'CommitID'
cloneAtCommit :: (Environment :> es) => Text -> CommitID -> FilePath -> Eff es CreateProcess
cloneAtCommit url commit path =
  withNonInteractiveSSH $
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
