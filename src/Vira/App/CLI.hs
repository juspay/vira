{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.App.CLI (
  -- * Types
  CLISettings (..),

  -- * Function
  parseCLI,
) where

import Data.Version (showVersion)
import Network.HostName (HostName, getHostName)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Paths_vira qualified
import Vira.Lib.TLS (TLSConfig, tlsConfigParser)
import Prelude hiding (Reader, reader, runReader)

-- | CLI Settings
data CLISettings = CLISettings
  { logLevel :: String
  -- ^ Minimum logging level
  , port :: Port
  -- ^ The port to bind the HTTP server to
  , host :: Text
  -- ^ The host to bind the HTTP server to
  , dbPath :: FilePath
  -- ^ Path to the vira db
  , instanceName :: Text
  -- ^ Name of the instance; uses hostname if unspecified
  , basePath :: Text
  -- ^ Base URL path for the http server
  , tlsConfig :: TLSConfig
  -- ^ TLS configuration for HTTPS support
  }
  deriving stock (Show)

-- | Parser for CLISettings
cliSettingsParser :: HostName -> Parser CLISettings
cliSettingsParser hostName = do
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
  dbPath <-
    strOption
      ( long "db-path"
          <> metavar "DB_PATH"
          <> help "Path to vira db"
          <> value "vira.db"
          <> showDefault
      )
  logLevel <-
    strOption
      ( long "log-level"
          <> metavar "LOG_LEVEL"
          <> help "Log level"
          <> value "Debug"
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
  pure CLISettings {..}

-- -- | Full parser with info
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

{- | Convert a git repository URL to a `State.Repo` record.
repoFromUrl :: Text -> Repo
repoFromUrl url =
  Repo (nameForGitUrl url) url
  where
    --  Convert a git repository URL to a name representing it.
    --
    --    For example, `https://github.com/user/foo.git` becomes `foo`.
    --
    nameForGitUrl :: Text -> RepoName
    nameForGitUrl gitUrl =
      let
        takeBaseName = T.reverse . T.takeWhile (/= '/') . T.reverse
        name = let s = takeBaseName gitUrl in fromMaybe s $ T.stripSuffix ".git" s
       in
        (fromString . toString) name
-}

-- defaultRepos :: [Text]
-- defaultRepos =
--   [ "https://github.com/srid/emanote.git"
--   , "https://github.com/juspay/omnix.git"
--   , "https://github.com/juspay/vira.git"
--   , "https://github.com/srid/haskell-flake.git"
--   , "https://github.com/srid/imako.git"
--   , "https://github.com/juspay/nixos-unified-template.git"
--   , "https://github.com/juspay/nix-common.git"
--   , "https://github.com/juspay/hyperswitch.git"
--   , "https://github.com/juspay/superposition.git"
--   , "https://github.com/juspay/services-flake.git"
--   ]
