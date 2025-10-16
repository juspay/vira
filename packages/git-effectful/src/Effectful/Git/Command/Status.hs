{- | Git status operations and parsing

Provides git status --porcelain=v2 functionality.
-}
module Effectful.Git.Command.Status (
  GitStatusPorcelain (..),
  gitStatusPorcelain,
  parseGitStatusPorcelain,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catchIO)
import Effectful.Git.Core (git)
import Effectful.Git.Logging (log)
import Effectful.Git.Types (BranchName (..))
import Effectful.Process (CreateProcess (..), Process, proc, readCreateProcess)

{- | Git status porcelain=v2 result

Represents the current branch and its working tree state.
-}
data GitStatusPorcelain = GitStatusPorcelain
  { branch :: BranchName
  , dirty :: Bool
  }
  deriving stock (Eq, Show)

{- | Get branch name and dirty status in one git call

Uses git status --porcelain=v2 to atomically retrieve both values,
avoiding race conditions and reducing git process overhead.
-}
gitStatusPorcelain :: (Error Text :> es, Log (RichMessage IO) :> es, Process :> es, IOE :> es) => FilePath -> Eff es GitStatusPorcelain
gitStatusPorcelain repoPath = do
  log Debug $ "Getting branch status in: " <> show repoPath
  output <-
    readCreateProcess (proc git ["status", "--porcelain=v2", "--branch", "--untracked-files=no"]) {cwd = Just repoPath} ""
      `catchIO` \ex -> do
        let errorMsg = "Git status --porcelain=v2 failed: " <> show ex
        log Error errorMsg
        throwError $ toText errorMsg

  case parseGitStatusPorcelain (toText output) of
    Left err -> throwError err
    Right result -> pure result

{- | Parse git status --porcelain=v2 output to extract GitStatusPorcelain

Returns Left with error message if branch.head line is missing.
-}
parseGitStatusPorcelain :: Text -> Either Text GitStatusPorcelain
parseGitStatusPorcelain output =
  let outputLines = lines output
      -- Extract branch name from "# branch.head <name>" line
      mBranchName =
        outputLines
          & mapMaybe (T.stripPrefix "# branch.head ")
          & listToMaybe
      -- Check if any non-header lines exist (indicating changes)
      isDirty = any (\line -> not (T.isPrefixOf "#" line) && not (T.null line)) outputLines
   in case mBranchName of
        Nothing -> Left "No branch.head found in git status output"
        Just branchName -> Right $ GitStatusPorcelain {branch = BranchName branchName, dirty = isDirty}
