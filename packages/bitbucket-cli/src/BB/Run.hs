-- | Command execution for bb CLI
module BB.Run (
  runBB,
) where

import BB.CLI (AuthArgs, CLISettings, Command (..), SignoffArgs, StatusArgs)
import BB.CLI qualified as CLI
import Bitbucket.API.V1.BuildStatus (BuildStatus (..))
import Bitbucket.API.V1.BuildStatus qualified as BS
import Bitbucket.API.V1.Core (BitbucketConfig (..))
import Bitbucket.API.V1.Core qualified as API
import Bitbucket.Config (ConfigError (..))
import Bitbucket.Config qualified as Config
import Bitbucket.ConfigPath (getConfigPath)
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
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
import Network.HTTP.Req (renderUrl)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | Main entry point for bb CLI
runBB :: IO ()
runBB = do
  -- Set proxy for Bitbucket API requests (comment out if not needed)
  -- setEnv "HTTPS_PROXY" "socks5://127.0.0.1:8080"

  settings <- CLI.parseCLI
  runCommand settings

-- | Run a command
runCommand :: CLISettings -> IO ()
runCommand settings =
  runEff . runLogActionStdout Info . ER.runReader (LogContext []) . runProcess $ do
    case CLI.command settings of
      SignoffCommand args -> runSignoffIO (CLI.force settings) args
      AuthCommand args -> liftIO $ runAuth args
      StatusCommand args -> liftIO $ runStatus args

-- | Run signoff with error handling
runSignoffIO :: (Log (RichMessage IO) :> es, ER.Reader LogContext :> es, Process :> es, IOE :> es) => Bool -> SignoffArgs -> Eff es ()
runSignoffIO forceFlag args = do
  result <- runErrorNoCallStack @Text $ runSignoff forceFlag args
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

  -- Load config for the detected Bitbucket host
  let expectedUrl = "https://" <> bitbucketHost
  configResult <- liftIO Config.loadConfig
  config <- case configResult of
    Left err -> liftIO $ die $ toString $ "Failed to load config: " <> showConfigError err <> "\nRun: bb auth --url " <> expectedUrl
    Right cfg -> do
      -- Verify config URL matches detected URL
      let configUrl = renderUrl (baseUrl cfg)
      when (configUrl /= expectedUrl) $
        liftIO $
          die $
            toString $
              "Config URL mismatch!\nExpected: " <> expectedUrl <> "\nFound in config: " <> configUrl <> "\nRun: bb auth --url " <> expectedUrl
      pure cfg

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
          { BS.state = CLI.state args
          , BS.key = CLI.key args
          , BS.name = CLI.name args
          , BS.url = CLI.url args
          , BS.description = CLI.description args
          }

  -- Post status
  liftIO $ BS.postBuildStatus config commitHash status

  liftIO $ putTextLn $ "✓ Signed off on " <> commitHash

-- | Run auth command
runAuth :: AuthArgs -> IO ()
runAuth args = do
  -- Prompt for token
  putStr "Enter your Bitbucket access token: "
  hFlush stdout
  token <- getLine

  -- Get config file path
  configPath <- getConfigPath

  -- Create directory if it doesn't exist
  createDirectoryIfMissing True (takeDirectory configPath)

  -- Write config
  let configContent = "baseUrl=" <> toString (CLI.baseUrl args) <> "\ntoken=" <> toString token
  writeFile configPath configContent

  putTextLn $ "✓ Configuration saved to " <> toText configPath

-- | Run status command
runStatus :: StatusArgs -> IO ()
runStatus args = do
  configPath <- getConfigPath
  Config.loadConfig >>= \case
    Left err -> do
      if CLI.jsonOutput args
        then do
          let json = object ["authenticated" .= False, "error" .= showConfigError err]
          putLBSLn $ Aeson.encode json
          exitFailure
        else do
          putTextLn $ "✗ " <> showConfigError err
          exitFailure
    Right config -> do
      -- Test connection
      API.testConnection config >>= \case
        Left testErr -> do
          if CLI.jsonOutput args
            then do
              let json =
                    object
                      [ "authenticated" .= False
                      , "config_found" .= True
                      , "connection_error" .= testErr
                      ]
              putLBSLn $ Aeson.encode json
              exitFailure
            else do
              putTextLn $ "✗ Connection test failed: " <> testErr
              exitFailure
        Right () -> do
          if CLI.jsonOutput args
            then do
              let json =
                    object
                      [ "authenticated" .= True
                      , "config_path" .= toText configPath
                      ]
              putLBSLn $ Aeson.encode json
            else do
              putTextLn "✓ Authenticated"
              putTextLn $ "  Config: " <> toText configPath

-- | Show config error
showConfigError :: ConfigError -> Text
showConfigError = \case
  ConfigFileNotFound path -> "Config file not found: " <> toText path
  ConfigParseError msg -> "Config parse error: " <> msg
  InvalidUrl url -> "Invalid URL: " <> url
