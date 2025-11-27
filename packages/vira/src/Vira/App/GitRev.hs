{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Runtime access to git commit ID set by Nix build wrapper.

Reads commit ID from environment variable set by wrapProgram.
-}
module Vira.App.GitRev (
  getCommitId,
)
where

import Effectful.Git.Types (CommitID (..))
import Relude

{- | Get git commit ID from NIX_GIT_HASH environment variable.

Returns Nothing if environment variable is not set (local development).
-}
getCommitId :: IO (Maybe CommitID)
getCommitId = fmap (CommitID . toText) <$> lookupEnv "NIX_GIT_HASH"
