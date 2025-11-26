{- | Bitbucket signoff functionality

Wrapper for bb signoff command.
-}
module Bitbucket.Signoff (
  Force (..),
  create,
) where

import System.Process (CreateProcess, proc)

-- | Force flag for signoff
data Force = Force | NoForce
  deriving stock (Show, Eq)

{- | Create bb signoff command

Creates a command to sign off on the current commit.
Always uses hardcoded values for state (successful), name, and description.

Arguments:
- 'FilePath': Path to bb executable
- 'Force': Whether to force signoff (skip dirty check)
- 'String': Context name (used as part of the key, e.g., "vira" -> "vira-signoff")
-}
create :: FilePath -> Force -> String -> CreateProcess
create bbBin force_ contextName =
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
