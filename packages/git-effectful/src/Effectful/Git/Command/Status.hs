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
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Colog.Simple.Process (withLogCommand)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catchIO)
import Effectful.Git.Core (git)
import Effectful.Git.Types (BranchName (..))
import Effectful.Process (CreateProcess (..), Process, proc, readCreateProcess)
import Effectful.Reader.Static qualified as ER

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
gitStatusPorcelain :: (Error Text :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es, Process :> es, IOE :> es) => FilePath -> Eff es GitStatusPorcelain
gitStatusPorcelain repoPath = do
  let statusCmd = proc git ["status", "--porcelain=v2", "--branch", "--untracked-files=no"]
  output <-
    withLogCommand statusCmd $ do
      log Debug "Running git status"
      readCreateProcess statusCmd {cwd = Just repoPath} ""
        `catchIO` \ex -> do
          throwError $ "Git status --porcelain=v2 failed: " <> show ex

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
