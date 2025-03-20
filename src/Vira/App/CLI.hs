{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.App.CLI (
  -- * Types
  Settings (..),
  RepoSettings (..),
  CachixSettings (..),

  -- * Function
  parseCLI,
) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Version (showVersion)
import Network.HostName (HostName, getHostName)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Paths_vira qualified
import Prelude hiding (Reader, reader, runReader)

{- | CLI settings
TODO: Use Severity from co-log
-}
data Settings = Settings
  { logLevel :: String
  -- ^ Minimum logging level
  , port :: Port
  -- ^ The port to bind the HTTP server to
  , host :: Text
  -- ^ The host to bind the HTTP server to
  , dbPath :: FilePath
  -- ^ Path to the vira db
  , repo :: RepoSettings
  -- ^ Repositories settings
  , instanceName :: Text
  -- ^ Name of the instance; uses hostname if unspecified
  , basePath :: Text
  -- ^ Base URL path for the http server
  }
  deriving stock (Show)

data RepoSettings = RepoSettings
  { cloneUrls :: [Text]
  -- ^ Repositories (git clone URL) to watch and build
  , branchWhitelist :: Set Text
  -- ^ Limit to building these branches to build
  , cachix :: Maybe CachixSettings
  -- ^ Cachix settings
  }
  deriving stock (Show)

data CachixSettings = CachixSettings
  { cachixName :: Text
  -- ^ Name of the cachix cache
  , authToken :: Text
  -- ^ Auth token for the cachix cache
  }
  deriving stock (Show)

defaultRepos :: [Text]
defaultRepos =
  [ "https://github.com/srid/emanote.git"
  , "https://github.com/juspay/omnix.git"
  , "https://github.com/juspay/vira.git"
  , "https://github.com/srid/haskell-flake.git"
  , "https://github.com/juspay/nix-common.git"
  , "https://github.com/juspay/hyperswitch.git"
  , "https://github.com/juspay/superposition.git"
  , "https://github.com/juspay/services-flake.git"
  ]

defaultBranchesToBuild :: Set Text
defaultBranchesToBuild = Set.fromList ["main", "master", "staging", "develop", "trunk"]

-- | Parser for Settings
settingsParser :: HostName -> Parser Settings
settingsParser hostName = do
  logLevel <-
    strOption
      ( long "log-level"
          <> metavar "LOG_LEVEL"
          <> help "Log level"
          <> value "Debug"
      )
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
          <> value "127.0.0.1"
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
  repo <- repoSettingsParser
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
  pure Settings {..}

-- | Parser for RepoSettings
repoSettingsParser :: Parser RepoSettings
repoSettingsParser = do
  cloneUrls <-
    option
      commaSeparatedTextList
      ( long "clone-urls"
          <> metavar "CLONE_URLS"
          <> help "Repositories to watch and build, as comma-separated `git clone` URLs"
          <> value defaultRepos
          <> showDefault
      )
  branchWhitelist <-
    option
      (Set.fromList <$> commaSeparatedTextList)
      ( long "branch-whitelist"
          <> metavar "BRANCH_WHITELIST"
          <> help "Limit to building these branches (comma-separated list)"
          <> value defaultBranchesToBuild
          <> showDefault
      )
  cachix <- optional cachixSettingsParser
  pure RepoSettings {..}

-- | Parser for CachixSettings
cachixSettingsParser :: Parser CachixSettings
cachixSettingsParser = do
  cachixName <-
    strOption
      ( long "cachix-name"
          <> metavar "CACHIX_NAME"
          <> help "Name of the cachix cache"
      )
  authToken <-
    strOption
      ( long "cachix-auth-token"
          <> metavar "CACHIX_AUTH_TOKEN"
          <> help "Auth token for the cachix cache"
      )
  pure CachixSettings {..}

-- | Full parser with info
parseSettings :: HostName -> ParserInfo Settings
parseSettings hostName =
  info
    (versionOption <*> settingsParser hostName <**> helper)
    ( fullDesc
        <> progDesc "Vira"
        <> header "vira - Nix CI for teams"
    )
  where
    versionOption =
      infoOption
        (showVersion Paths_vira.version)
        (long "version" <> help "Show version")

-- | Helper to parse comma-separated lists into Text values
commaSeparatedTextList :: ReadM [Text]
commaSeparatedTextList = eitherReader $ \s -> do
  let split = filter (not . T.null) . T.splitOn "," . toText
  pure (split s)

parseCLI :: IO Settings
parseCLI = do
  hostName <- liftIO getHostName
  execParser $ parseSettings hostName
