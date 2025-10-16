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

import Colog.Core (Severity (..))
import Data.Char (toLower)
import Data.Version (showVersion)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.WarpTLS.Simple (TLSConfig, tlsConfigParser)
import Options.Applicative hiding (command)
import Options.Applicative qualified as OA
import Paths_vira qualified
import Prelude hiding (Reader, reader, runReader)

-- | Global CLI Settings
data GlobalSettings = GlobalSettings
  { logLevel :: Severity
  -- ^ Minimum logging level
  , stateDir :: FilePath
  -- ^ Directory where Vira stores its state
  , autoResetState :: Bool
  -- ^ Automatically reset state on schema mismatch (removes ViraState and job workspaces)
  }
  deriving stock (Show)

-- | Web server settings
data WebSettings = WebSettings
  { port :: Port
  -- ^ The port to bind the HTTP server to
  , host :: Text
  -- ^ The host to bind the HTTP server to
  , basePath :: Text
  -- ^ Base URL path for the http server
  , tlsConfig :: TLSConfig
  -- ^ TLS configuration for HTTPS support
  , importFile :: Maybe FilePath
  -- ^ Optional JSON file to import on startup
  }
  deriving stock (Show)

-- | Available commands
data Command
  = WebCommand WebSettings
  | ExportCommand
  | ImportCommand
  | InfoCommand
  | CICommand (Maybe FilePath)
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
    option
      severityReader
      ( long "log-level"
          <> metavar "LOG_LEVEL"
          <> help "Minimum log level (Debug, Info, Warning, Error)"
          <> value Info
          <> showDefault
      )
  autoResetState <-
    switch
      ( long "auto-reset-state"
          <> help "Automatically reset state on schema mismatch (removes ViraState and job workspaces)"
      )
  pure GlobalSettings {..}

-- | Reader for parsing severity levels
severityReader :: ReadM Severity
severityReader = eitherReader $ \s -> case map toLower s of
  "debug" -> Right Debug
  "info" -> Right Info
  "warning" -> Right Warning
  "warn" -> Right Warning -- Allow both variants
  "error" -> Right Error
  _ -> Left "Invalid log level. Choose from: Debug, Info, Warning, Error"

-- | Parser for web settings
webSettingsParser :: Parser WebSettings
webSettingsParser = do
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
  basePath <-
    strOption
      ( long "base-path"
          <> metavar "BASE_PATH"
          <> help "Base URL path for the http server"
          <> value "/"
          <> showDefault
      )
  tlsConfig <- tlsConfigParser
  importFile <-
    optional $
      strOption
        ( long "import"
            <> metavar "FILE"
            <> help "Import JSON file on startup"
        )
  pure WebSettings {..}

-- | Parser for CI command
ciCommandParser :: Parser Command
ciCommandParser =
  CICommand
    <$> optional
      ( strArgument
          ( metavar "DIRECTORY"
              <> help "Directory to run CI in (defaults to current directory)"
          )
      )

-- | Parser for commands
commandParser :: Parser Command
commandParser =
  hsubparser
    ( OA.command "web" (info (WebCommand <$> webSettingsParser) (progDesc "Start the web server"))
        <> OA.command "export" (info (pure ExportCommand) (progDesc "Export Vira state to JSON"))
        <> OA.command "import" (info (pure ImportCommand) (progDesc "Import Vira state from JSON"))
        <> OA.command "info" (info (pure InfoCommand) (progDesc "Show Vira information (version, schema version, etc.)"))
        <> OA.command "ci" (info ciCommandParser (progDesc "Run CI pipeline in a directory"))
    )

-- | Parser for CLISettings
cliSettingsParser :: Parser CLISettings
cliSettingsParser = do
  globalSettings <- globalSettingsParser
  command <- commandParser
  pure CLISettings {..}

-- | Full parser with info
parseCLISettings :: ParserInfo CLISettings
parseCLISettings =
  info
    (versionOption <*> cliSettingsParser <**> helper)
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
parseCLI = execParser parseCLISettings
