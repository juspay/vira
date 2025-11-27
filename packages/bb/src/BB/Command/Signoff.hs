{-# LANGUAGE OverloadedRecordDot #-}

-- | Signoff command implementation
module BB.Command.Signoff (
  runSignoff,
) where

import BB.Config qualified as Config
import Bitbucket.API.V1.BuildStatus (BuildStatus (..))
import Bitbucket.API.V1.BuildStatus qualified as BS
import Bitbucket.API.V1.Core (ServerEndpoint (..))
import Colog.Message (RichMessage)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
import Effectful.Error.Static (Error, throwError)
import Effectful.Git.Command.Remote qualified as Git
import Effectful.Git.Command.RevParse qualified as Git
import Effectful.Git.Command.Status qualified as Git
import Effectful.Git.Platform (GitPlatform (..), detectPlatform)
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER

-- | Run signoff command
runSignoff :: (Error Text :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es, Process :> es, IOE :> es) => Bool -> BuildStatus -> Eff es ()
runSignoff forceFlag status = do
  -- Get git remote URL (from current directory)
  remoteUrl <- Git.getRemoteUrl "." "origin"

  -- Detect platform and get stripped URL
  bitbucketHost <- case detectPlatform remoteUrl of
    Just (Bitbucket host) -> pure host
    Just GitHub -> throwError @Text "Error: GitHub repositories not supported by bb CLI (use gh instead)"
    Nothing -> throwError @Text $ "Error: Could not detect Bitbucket platform from remote URL: " <> remoteUrl

  -- Load config and lookup server for this host
  let endpoint = ServerEndpoint bitbucketHost
  configResult <- liftIO Config.loadConfig
  serverConfig <- case configResult of
    Left err -> throwError @Text $ "Failed to load config: " <> err
    Right servers -> case Config.lookupServer endpoint servers of
      Nothing ->
        throwError @Text $ "Server not configured: " <> bitbucketHost
      Just cfg -> pure cfg

  -- Check working directory is clean (no uncommitted or unpushed changes)
  unless forceFlag $ do
    -- Check for uncommitted changes
    statusResult <- Git.gitStatusPorcelain "."
    let uncommitted = statusResult.dirty
    -- Check for unpushed commits
    unpushed <- Git.hasUnpushedCommits "."
    when (uncommitted || unpushed) $
      throwError @Text "Error: repository has uncommitted or unpushed changes (use -f to force)"

  -- Get current commit hash
  commitHash <- Git.getCurrentCommit "."

  -- Post status
  BS.postBuildStatus endpoint serverConfig.token commitHash status

  liftIO $ putTextLn $ "✓ Signed off on " <> commitHash
