{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Core GitHub CLI functionality

Provides the path to the gh binary.
-}
module GH.Core (
  ghBin,
  GitHubRepo (..),
  PullRequest (..),
  PullRequestLookup (..),
  githubRepoFromUrl,
  lookupForBranch,
  lookupForBranchFromCloneUrl,
) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Which (staticWhich)
import Text.URI (Authority (..))
import Text.URI qualified as URI

{- | Path to the @gh@ executable

This should be available in the PATH, thanks to Nix and 'System.Which.staticWhich'.
-}
ghBin :: FilePath
ghBin = $(staticWhich "gh")

-- | A GitHub repository address.
data GitHubRepo = GitHubRepo
  { owner :: Text
  , repository :: Text
  }
  deriving stock (Show, Eq)

-- | Pull request details used by Vira's branch and job pages.
data PullRequest = PullRequest
  { number :: Natural
  , title :: Text
  , url :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Result of looking up a pull request for a branch.
data PullRequestLookup
  = UnsupportedRepository
  | NoPullRequest
  | FoundPullRequest PullRequest
  | PullRequestLookupFailed Text
  deriving stock (Show, Eq)

-- | Extract the owner and repository name from a GitHub clone URL.
githubRepoFromUrl :: Text -> Maybe GitHubRepo
githubRepoFromUrl raw = do
  uri <- rightToMaybe $ URI.mkURI $ normalizeGitUrl raw
  host <- hostFromUri uri
  guard $ isGitHubHost host
  (_isAbsolute, pathSegments) <- URI.uriPath uri
  case toList $ URI.unRText <$> pathSegments of
    owner : repoSegment : _ -> do
      owner' <- nonEmptyText owner
      repository' <- nonEmptyText $ fromMaybe repoSegment $ T.stripSuffix ".git" repoSegment
      pure GitHubRepo {owner = owner', repository = repository'}
    _ -> Nothing
  where
    nonEmptyText txt =
      if T.null txt
        then Nothing
        else Just txt

-- | Find the pull request associated with a branch in a GitHub clone URL.
lookupForBranchFromCloneUrl :: Text -> Text -> IO PullRequestLookup
lookupForBranchFromCloneUrl cloneUrl branch =
  case githubRepoFromUrl cloneUrl of
    Nothing -> pure UnsupportedRepository
    Just repo -> lookupForBranch repo branch

-- | Find the pull request associated with a branch in a GitHub repository.
lookupForBranch :: GitHubRepo -> Text -> IO PullRequestLookup
lookupForBranch repo branch = do
  result <-
    try @IOException $
      readProcessWithExitCode
        ghBin
        ["pr", "list", "--repo", toString $ githubRepoSlug repo, "--head", toString branch, "--json", "number,title,url", "--limit", "1"]
        ""
  pure $ case result of
    Left ex -> PullRequestLookupFailed $ "gh pr list failed to start: " <> toText (displayException ex)
    Right (exitCode, stdoutText, stderrText) ->
      parseGhResult exitCode (toText stdoutText) (toText stderrText)

parseGhResult :: ExitCode -> Text -> Text -> PullRequestLookup
parseGhResult exitCode stdoutText stderrText =
  case exitCode of
    ExitSuccess ->
      case Aeson.eitherDecodeStrict @[PullRequest] (encodeUtf8 stdoutText) of
        Left err -> PullRequestLookupFailed $ "gh pr list returned invalid JSON: " <> toText err
        Right [] -> NoPullRequest
        Right (pullRequest : _) -> FoundPullRequest pullRequest
    ExitFailure code ->
      PullRequestLookupFailed $ "gh pr list exited with " <> show code <> ": " <> combinedOutput
  where
    combinedOutput = T.strip $ stdoutText <> "\n" <> stderrText

normalizeGitUrl :: Text -> Text
normalizeGitUrl raw
  | "://" `T.isInfixOf` raw = raw
  | otherwise =
      case T.breakOn ":" raw of
        (before, after)
          | T.null after -> raw
          | otherwise -> "ssh://" <> before <> "/" <> T.drop 1 after

hostFromUri :: URI.URI -> Maybe Text
hostFromUri uri =
  case URI.uriAuthority uri of
    Right auth -> Just $ URI.unRText $ authHost auth
    Left _ -> Nothing

isGitHubHost :: Text -> Bool
isGitHubHost =
  (== "github.com")

githubRepoSlug :: GitHubRepo -> Text
githubRepoSlug GitHubRepo {owner, repository} =
  owner <> "/" <> repository
