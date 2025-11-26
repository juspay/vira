{- | Module for working with Git repositories in Haskell

A standalone library for git operations using the effectful library.
Servant instances should be gated behind a Cabal flag.
-}
module Effectful.Git (
  -- * Git executable
  git,

  -- * Types
  module Effectful.Git.Types,

  -- * Platform Detection
  module Effectful.Git.Platform,

  -- * Commands
  module Effectful.Git.Command.Remote,
) where

import Effectful.Git.Command.Remote
import Effectful.Git.Core (git)
import Effectful.Git.Platform
import Effectful.Git.Types
