{-# LANGUAGE TemplateHaskell #-}

{- | Core git executable path and SSH configuration

Provides the git executable path and SSH configuration helpers for other modules.
-}
module Effectful.Git.Core (
  git,
  withNonInteractiveSSH,
) where

import Effectful (Eff, (:>))
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Env
import Effectful.Process (CreateProcess (..))
import System.Which (staticWhich)

{- | Path to the `git` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
git :: FilePath
git = $(staticWhich "git")

{- | Add non-interactive SSH configuration to a git command.

This prevents git from blocking on interactive SSH prompts (host verification, password entry).
Merges with any existing GIT_SSH_COMMAND in the environment and preserves all other env vars.

Uses:
- BatchMode=yes: Disables all interactive prompts
- StrictHostKeyChecking=accept-new: Auto-accepts new unknown hosts, rejects changed keys
-}
withNonInteractiveSSH :: (Environment :> es) => CreateProcess -> Eff es CreateProcess
withNonInteractiveSSH cmd = do
  -- Get current environment
  currentEnv <- Env.getEnvironment

  -- Look up existing GIT_SSH_COMMAND
  existingSSH <- Env.lookupEnv "GIT_SSH_COMMAND"

  -- Build our SSH command, merging with user's if it exists
  let ourOptions = "-o BatchMode=yes -o StrictHostKeyChecking=accept-new"
      sshCommand = case existingSSH of
        Nothing -> "ssh " <> ourOptions
        Just userCmd -> toText userCmd <> " " <> ourOptions

  -- Update environment with merged SSH command (remove old one first to avoid duplicates)
  let updatedEnv = ("GIT_SSH_COMMAND", toString sshCommand) : filter ((/= "GIT_SSH_COMMAND") . fst) currentEnv

  pure $ cmd {env = Just updatedEnv}
