{-# LANGUAGE OverloadedRecordDot #-}

-- | Command execution for bb CLI
module BB.Run (
  runBB,
) where

import BB.CLI (AuthArgs, CLISettings, Command (..), SignoffArgs, StatusArgs)
import BB.CLI qualified as CLI
import BB.Config (ConfigError (..), ServerConfig (..), getConfigPath)
import BB.Config qualified as Config
import Bitbucket.API.V1.BuildStatus (BuildStatus (..))
import Bitbucket.API.V1.BuildStatus qualified as BS
import Bitbucket.API.V1.Core (ServerEndpoint (..), Token (..))
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), runLogActionStdout)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Git.Command.Remote qualified as Git
import Effectful.Git.Command.RevParse qualified as Git
import Effectful.Git.Command.Status qualified as Git
import Effectful.Git.Core (git)
import Effectful.Git.Platform (GitPlatform (..), detectPlatform)
import Effectful.Process (Process, proc, readCreateProcess, runProcess)
import Effectful.Reader.Static qualified as ER
import Text.URI (Authority (..), mkURI, unRText)
import Text.URI qualified as URI

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
    result <- runErrorNoCallStack @Text $ do
      case CLI.command settings of
        SignoffCommand args -> runSignoff (CLI.force settings) args
        AuthCommand args -> runAuth args
        StatusCommand args -> runStatus args
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
  configResult <- liftIO Config.loadConfig
  serverConfig <- case configResult of
    Left err -> liftIO $ die $ toString $ "Failed to load config: " <> showConfigError err <> "\nRun: bb auth " <> bitbucketHost
    Right servers -> case Config.lookupServer endpoint servers of
      Nothing ->
        liftIO $ die $ toString $ "Server not configured: " <> bitbucketHost <> "\nRun: bb auth " <> bitbucketHost
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
          { BS.state = CLI.state args
          , BS.key = CLI.key args
          , BS.name = CLI.name args
          , BS.url = CLI.url args
          , BS.description = CLI.description args
          }

  -- Post status
  BS.postBuildStatus endpoint serverConfig.token commitHash status

  liftIO $ putTextLn $ "✓ Signed off on " <> commitHash

-- | Run auth command
runAuth :: (Error Text :> es, IOE :> es) => AuthArgs -> Eff es ()
runAuth args = do
  -- Prompt for token
  liftIO $ putStr "Enter your Bitbucket access token: "
  liftIO $ hFlush stdout
  tokenInput <- liftIO getLine

  -- Parse URL to extract host
  let baseUrlText = CLI.baseUrl args
      -- Normalize URL: prepend https:// if no protocol specified
      normalizedUrl =
        if "http://" `isPrefixOf` toString baseUrlText || "https://" `isPrefixOf` toString baseUrlText
          then baseUrlText
          else "https://" <> baseUrlText

  uri <- case mkURI normalizedUrl of
    Left parseErr ->
      throwError $ "✗ URL parse error for " <> baseUrlText <> ": " <> show parseErr
    Right u -> pure u

  -- Extract host from URI
  host <- case URI.uriAuthority uri of
    Right auth -> pure $ unRText (authHost auth)
    Left _ ->
      throwError $ "✗ Could not extract host from URL: " <> normalizedUrl

  -- Load existing config or start fresh
  existingServers <- liftIO $ whenRightM Map.empty Config.loadConfig pure

  -- Insert/update server
  let endpoint = ServerEndpoint host
      serverConfig = ServerConfig {token = Token (toText tokenInput)}
      updatedServers = Map.insert endpoint serverConfig existingServers

  -- Save config
  liftIO $ Config.saveConfig updatedServers

  configPath <- liftIO getConfigPath
  liftIO $ putTextLn $ "✓ Configuration saved to " <> toText configPath

-- | Run status command
runStatus :: (Error Text :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es, IOE :> es) => StatusArgs -> Eff es ()
runStatus args = do
  configPath <- liftIO getConfigPath
  liftIO Config.loadConfig >>= \case
    Left err -> do
      if CLI.jsonOutput args
        then do
          let json = object ["authenticated" .= False, "error" .= showConfigError err]
          liftIO $ putLBSLn $ Aeson.encode json
          throwError @Text "Config error"
        else do
          liftIO $ putTextLn $ "✗ " <> showConfigError err
          throwError @Text "Config error"
    Right servers -> do
      if Map.null servers
        then do
          if CLI.jsonOutput args
            then do
              let json = object ["authenticated" .= False, "error" .= ("No servers configured" :: Text)]
              liftIO $ putLBSLn $ Aeson.encode json
              throwError @Text "No servers configured"
            else do
              liftIO $ putTextLn "✗ No servers configured"
              throwError @Text "No servers configured"
        else do
          -- Test first server (could be enhanced to test all or prompt user)
          case viaNonEmpty head (Map.toList servers) of
            Nothing -> do
              if CLI.jsonOutput args
                then do
                  let json = object ["authenticated" .= False, "error" .= ("No servers configured" :: Text)]
                  liftIO $ putLBSLn $ Aeson.encode json
                  throwError @Text "No servers configured"
                else do
                  liftIO $ putTextLn "✗ No servers configured"
                  throwError @Text "No servers configured"
            Just (endpoint, serverConfig) ->
              Config.testConnection endpoint serverConfig.token >>= \case
                Left testErr -> do
                  if CLI.jsonOutput args
                    then do
                      let json =
                            object
                              [ "authenticated" .= False
                              , "config_found" .= True
                              , "connection_error" .= testErr
                              ]
                      liftIO $ putLBSLn $ Aeson.encode json
                      throwError @Text testErr
                    else do
                      liftIO $ putTextLn $ "✗ Connection test failed: " <> testErr
                      throwError @Text testErr
                Right () -> do
                  if CLI.jsonOutput args
                    then do
                      let json =
                            object
                              [ "authenticated" .= True
                              , "config_path" .= toText configPath
                              , "server_count" .= Map.size servers
                              ]
                      liftIO $ putLBSLn $ Aeson.encode json
                    else do
                      liftIO $ putTextLn "✓ Authenticated"
                      liftIO $ putTextLn $ "  Config: " <> toText configPath
                      liftIO $ putTextLn $ "  Servers: " <> show (Map.size servers)

-- | Show config error
showConfigError :: ConfigError -> Text
showConfigError = \case
  ConfigFileNotFound path -> "Config file not found: " <> toText path
  JsonDecodeError msg -> "JSON decode error: " <> msg
