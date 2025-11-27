{-# LANGUAGE OverloadedRecordDot #-}

-- | Command execution for bb CLI
module BB.Run (
  runBB,
) where

import BB.CLI.Core (AuthCommand (..), CLISettings, Command (..), SignoffArgs)
import BB.CLI.Core qualified as CLI
import BB.Command.Auth.Login qualified as Login
import BB.Command.Auth.Status qualified as Status
import BB.Config (ConfigError (..))
import BB.Config qualified as Config
import Bitbucket.API.V1.BuildStatus (BuildStatus (..))
import Bitbucket.API.V1.BuildStatus qualified as BS
import Bitbucket.API.V1.Core (ServerEndpoint (..))
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), runLogActionStdout)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Git.Command.Remote qualified as Git
import Effectful.Git.Command.RevParse qualified as Git
import Effectful.Git.Command.Status qualified as Git
import Effectful.Git.Core (git)
import Effectful.Git.Platform (GitPlatform (..), detectPlatform)
import Effectful.Process (Process, proc, readCreateProcess, runProcess)
import Effectful.Reader.Static qualified as ER

-- | Main entry point for bb CLI
runBB :: IO ()
runBB = do
  settings <- CLI.parseCLI
  runCommand settings

-- | Run a command
runCommand :: CLISettings -> IO ()
runCommand settings =
  runEff . runLogActionStdout Info . ER.runReader (LogContext []) . runProcess $ do
    result <- runErrorNoCallStack @Text $ do
      case settings.command of
        SignoffCommand args -> runSignoff settings.force args
        AuthCommand authCmd -> case authCmd of
          LoginCommand {baseUrl} -> Login.runLogin baseUrl
          StatusCommand {jsonOutput} -> Status.runStatus jsonOutput
    either (liftIO . die . toString @Text) pure result

-- | Run signoff command
runSignoff :: (Error Text :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es, Process :> es, IOE :> es) => Bool -> SignoffArgs -> Eff es ()
runSignoff forceFlag args = do
  -- Get git remote URL (from current directory)
  remoteUrl <- Git.getRemoteUrl "." "origin"

  -- Detect platform and get stripped URL
  bitbucketHost <- case detectPlatform remoteUrl of
    Just (Bitbucket host) -> pure host
    Just GitHub -> liftIO $ die "Error: GitHub repositories not supported by bb CLI (use gh instead)"
    Nothing -> liftIO $ die $ "Error: Could not detect Bitbucket platform from remote URL: " <> toString remoteUrl

  -- Load config and lookup server for this host
  let endpoint = ServerEndpoint bitbucketHost
      authHelp = "\nCreate a token in Bitbucket (Account → HTTP access tokens) with 'Repository write' permission, then run: bb auth login " <> bitbucketHost
  configResult <- liftIO Config.loadConfig
  serverConfig <- case configResult of
    Left err -> liftIO $ die $ toString $ "Failed to load config: " <> showConfigError err <> authHelp
    Right servers -> case Config.lookupServer endpoint servers of
      Nothing ->
        liftIO $ die $ toString $ "Server not configured: " <> bitbucketHost <> authHelp
      Just cfg -> pure cfg

  -- Check working directory is clean (no uncommitted or unpushed changes)
  unless forceFlag $ do
    -- Check for uncommitted changes
    statusOutput <- readCreateProcess (proc git ["-C", ".", "status", "--porcelain"]) ""
    let uncommitted = statusOutput /= ""
    -- Check for unpushed commits
    unpushed <- Git.hasUnpushedCommits "."
    when (uncommitted || unpushed) $
      liftIO $
        die $
          toString @Text "Error: repository has uncommitted or unpushed changes (use -f to force)"

  -- Get current commit hash
  commitHash <- Git.getCurrentCommit "."

  -- Build status
  let status =
        BuildStatus
          { BS.state = args.state
          , BS.key = args.key
          , BS.name = args.name
          , BS.url = args.url
          , BS.description = args.description
          }

  -- Post status
  BS.postBuildStatus endpoint serverConfig.token commitHash status

  liftIO $ putTextLn $ "✓ Signed off on " <> commitHash

-- | Show config error
showConfigError :: ConfigError -> Text
showConfigError = \case
  ConfigFileNotFound path -> "Config file not found: " <> toText path
  JsonDecodeError msg -> "JSON decode error: " <> msg
