{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.App.CLI (
  -- * Types
  CLISettings (..),
  GlobalSettings (..),
  WebSettings (..),
  Command (..),

  -- * Functions
  parseCLI,
) where

import Data.Version (showVersion)
import Network.HostName (HostName, getHostName)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.WarpTLS.Simple (TLSConfig, tlsConfigParser)
import Options.Applicative hiding (command)
import Options.Applicative qualified as OA
import Paths_vira qualified
import Prelude hiding (Reader, reader, runReader)

-- | Global CLI Settings
data GlobalSettings = GlobalSettings
  { logLevel :: String
  -- ^ Minimum logging level
  , stateDir :: FilePath
  -- ^ Directory where Vira stores its state
  }
  deriving stock (Show)

-- | Web server settings
data WebSettings = WebSettings
  { port :: Port
  -- ^ The port to bind the HTTP server to
  , host :: Text
  -- ^ The host to bind the HTTP server to
  , instanceName :: Text
  -- ^ Name of the instance; uses hostname if unspecified
  , basePath :: Text
  -- ^ Base URL path for the http server
  , tlsConfig :: TLSConfig
  -- ^ TLS configuration for HTTPS support
  }
  deriving stock (Show)

-- | Available commands
newtype Command
  = WebCommand WebSettings
  deriving stock (Show)

-- | Complete CLI configuration
data CLISettings = CLISettings
  { globalSettings :: GlobalSettings
  , command :: Command
  }
  deriving stock (Show)

-- | Parser for global settings
globalSettingsParser :: Parser GlobalSettings
globalSettingsParser = do
  stateDir <-
    strOption
      ( long "state-dir"
          <> metavar "STATE_DIR"
          <> help "Directory where Vira stores its state"
          <> value "./state"
          <> showDefault
      )
  logLevel <-
    strOption
      ( long "log-level"
          <> metavar "LOG_LEVEL"
          <> help "Log level"
          <> value "Debug"
      )
  pure GlobalSettings {..}

-- | Parser for web settings
webSettingsParser :: HostName -> Parser WebSettings
webSettingsParser hostName = do
  port <-
    option
      auto
      ( long "port"
          <> metavar "PORT"
          <> help "Port to bind the HTTP server to"
          <> value 5005
      )
  host <-
    strOption
      ( long "host"
          <> metavar "HOST"
          <> help "Host"
          <> value "0.0.0.0"
          <> showDefault
      )
  instanceName <-
    strOption
      ( long "instance-name"
          <> metavar "INSTANCE_NAME"
          <> help "Name of the instance"
          <> value (toText hostName)
          <> showDefault
      )
  basePath <-
    strOption
      ( long "base-path"
          <> metavar "BASE_PATH"
          <> help "Base URL path for the http server"
          <> value "/"
          <> showDefault
      )
  tlsConfig <- tlsConfigParser
  pure WebSettings {..}

-- | Parser for commands
commandParser :: HostName -> Parser Command
commandParser hostName =
  hsubparser
    ( OA.command "web" (info (WebCommand <$> webSettingsParser hostName) (progDesc "Start the web server"))
    )

-- | Parser for CLISettings
cliSettingsParser :: HostName -> Parser CLISettings
cliSettingsParser hostName = do
  globalSettings <- globalSettingsParser
  command <- commandParser hostName
  pure CLISettings {..}

-- | Full parser with info
parseCLISettings :: HostName -> ParserInfo CLISettings
parseCLISettings hostName =
  info
    (versionOption <*> cliSettingsParser hostName <**> helper)
    ( fullDesc
        <> progDesc "Vira"
        <> header "vira - Nix CI for teams"
    )
  where
    versionOption =
      infoOption
        (showVersion Paths_vira.version)
        (long "version" <> help "Show version")

parseCLI :: IO CLISettings
parseCLI = do
  hostName <- liftIO getHostName
  execParser $ parseCLISettings hostName
