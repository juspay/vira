{- | CLI invocation wrappers for use by vira

Provides wrappers for bb commands that shell out via process invocation.
-}
module BB.CLI.Invocation (
  Force (..),
  createSignoff,
) where

import System.Process (CreateProcess, proc)

-- | Force flag for signoff
data Force = Force | NoForce
  deriving stock (Show, Eq)

{- | Create bb signoff commands

Creates commands to sign off on the current commit, one for each context name.
Always uses hardcoded values for state (successful), name, and description.

Arguments:
- 'FilePath': Path to bb executable
- 'Force': Whether to force signoff (skip dirty check)
- 'NonEmpty String': Context names (e.g., ["vira/x86_64-linux", "vira/aarch64-darwin"])
-}
createSignoff :: FilePath -> Force -> NonEmpty String -> NonEmpty CreateProcess
createSignoff bbBin force_ =
  fmap createOne
  where
    createOne contextName =
      proc bbBin $
        ["-f" | force_ == Force]
          <> [ "signoff"
             , "--state"
             , "successful"
             , "--key"
             , contextName <> "-signoff"
             , "--name"
             , contextName
             , "--description"
             , "Build succeeded"
             ]
