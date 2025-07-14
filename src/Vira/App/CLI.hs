{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.App.CLI (
  -- * Types
  Settings (..),
  ReposSettings (..),
  RepoSettings (..),
  CachixSettings (..),
  AtticSettings (..),

  -- * Function
  parseCLI,
) where

import Data.Text qualified as T
import Data.Version (showVersion)
import Network.HostName (HostName, getHostName)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Paths_vira qualified
import Vira.Lib.Attic (AtticCache, AtticServer (AtticServer), AtticToken)
import Vira.Lib.TLS (TLSConfig, tlsConfigParser)
import Vira.State.Type (Repo (Repo), RepoName)
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
  , repo :: ReposSettings
  -- ^ Repositories settings
  , instanceName :: Text
  -- ^ Name of the instance; uses hostname if unspecified
  , basePath :: Text
  -- ^ Base URL path for the http server
  , tlsConfig :: TLSConfig
  -- ^ TLS configuration for HTTPS support
  }
  deriving stock (Show)

data RepoSettings = RepoSettings
  { repoInfo :: Repo
  -- ^ The repository information (name and clone URL)
  , dummy :: ()
  -- ^ Placeholder for future per-repo settings)
  }
  deriving stock (Show)

data ReposSettings = ReposSettings
  { repoSettings :: [RepoSettings]
  -- ^ List of repository settings
  , cachix :: Maybe CachixSettings
  -- ^ Default Cachix settings
  , attic :: Maybe AtticSettings
  -- ^ Default Attic settings
  }
  deriving stock (Show)

data AtticSettings = AtticSettings
  { atticServer :: AtticServer
  -- ^ Attic server information
  , atticCacheName :: AtticCache
  -- ^ Name of the attic cache
  , atticToken :: AtticToken
  -- ^ Access token for `atticServerUrl`
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
  , "https://github.com/srid/imako.git"
  , "https://github.com/juspay/nixos-unified-template.git"
  , "https://github.com/juspay/nix-common.git"
  , "https://github.com/juspay/hyperswitch.git"
  , "https://github.com/juspay/superposition.git"
  , "https://github.com/juspay/services-flake.git"
  ]

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
  repo <- reposSettingsParser
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
  pure Settings {..}

-- | Parser for ReposSettings
reposSettingsParser :: Parser ReposSettings
reposSettingsParser =
  buildReposSettings <$> parseCloneUrls <*> parseCachix <*> parseAttic
  where
    -- For backwards compatibility, we support --clone-urls
    parseCloneUrls =
      fmap repoFromUrl
        <$> option
          commaSeparatedTextList
          ( long "clone-urls"
              <> metavar "CLONE_URLS"
              <> help "Repositories to watch and build, as comma-separated `git clone` URLs"
              <> value defaultRepos
              <> showDefault
          )

    parseCachix = optional cachixSettingsParser

    parseAttic = optional atticSettingsParser

    buildReposSettings cloneUrls cachix attic =
      let repoSettings = map (\repoInfo -> RepoSettings {repoInfo, dummy = ()}) cloneUrls
       in ReposSettings {repoSettings, cachix, attic}

-- | Parser for AtticSettings
atticSettingsParser :: Parser AtticSettings
atticSettingsParser = do
  atticServer <-
    AtticServer
      <$> strOption
        ( long "attic-server-name"
            <> metavar "ATTIC_SERVER_NAME"
            <> help "A shorthand for ATTIC_SERVER_URL"
        )
      <*> strOption
        ( long "attic-server-url"
            <> metavar "ATTIC_SERVER_URL"
            <> help "Server address (e.g., https://cache.example.org)"
        )
  atticCacheName <-
    strOption
      ( long "attic-cache-name"
          <> metavar "ATTIC_CACHE_NAME"
          <> help "Name of the cache hosted in ATTIC_SERVER_URL"
      )
  atticToken <-
    strOption
      ( long "attic-token"
          <> metavar "ATTIC_TOKEN"
          <> help "Access token for ATTIC_SERVER_URL"
      )
  pure AtticSettings {..}

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

-- | Convert a git repository URL to a `State.Repo` record.
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
