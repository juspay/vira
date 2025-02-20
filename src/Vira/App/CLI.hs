{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.App.CLI where

import Data.Set qualified as Set
import Network.Wai.Handler.Warp (Port)
import OptEnvConf
import Prelude hiding (Reader, reader)

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

instance HasParser Settings where
  settingsParser = withoutConfig $ do
    logLevel <-
      setting
        [ reader str
        , metavar "LOG_LEVEL"
        , help "Log level"
        , name "log-level"
        , value "Debug"
        ]
    port <-
      setting
        [ reader auto
        , metavar "PORT"
        , help "Port to bind the HTTP server to"
        , name "port"
        , value 5005
        ]
    host <-
      setting
        [ reader str
        , metavar "HOST"
        , help "Host"
        , name "host"
        , value "127.0.0.1"
        ]
    dbPath <-
      setting
        [ reader str
        , metavar "DB_PATH"
        , help "Path to vira db"
        , name "db-path"
        , value "vira.db"
        ]
    repo <- subSettings "repo"
    instanceName <-
      setting
        [ reader str
        , metavar "INSTANCE_NAME"
        , help "Name of the instance; uses hostname if unspecified"
        , name "instance-name"
        , value ""
        ]
    pure Settings {..}

instance HasParser RepoSettings where
  settingsParser = withoutConfig $ do
    cloneUrls <-
      setting
        [ reader (commaSeparatedList $ fmap (toText @String) str)
        , metavar "CLONE_URLS"
        , help "Repositories (git clone URL) to watch and build"
        , name "clone-urls"
        , value defaultRepos
        ]
    branchWhitelist <-
      setting
        [ reader (Set.fromList <$> commaSeparatedList str)
        , metavar "BRANCH_WHITELIST"
        , help "Limit to building these branches"
        , name "branch-whitelist"
        , value defaultBranchesToBuild
        ]
    cachix <- optional $ subSettings "cachix"
    pure RepoSettings {..}

instance HasParser CachixSettings where
  settingsParser = withoutConfig $ do
    cachixName <-
      setting
        [ reader str
        , metavar "CACHIX_NAME"
        , help "Name of the cachix cache"
        , name "name"
        ]
    authToken <-
      setting
        [ reader str
        , metavar "CACHIX_AUTH_TOKEN"
        , help "Auth token for the cachix cache"
        , name "auth-token"
        ]
    pure CachixSettings {..}

defaultRepos :: [Text]
defaultRepos =
  [ "https://github.com/srid/emanote.git"
  , "https://github.com/juspay/omnix.git"
  , "https://github.com/juspay/vira.git"
  , "https://github.com/srid/haskell-flake.git"
  , "https://github.com/juspay/hyperswitch.git"
  , "https://github.com/juspay/superposition.git"
  , "https://github.com/juspay/services-flake.git"
  ]

defaultBranchesToBuild :: Set Text
defaultBranchesToBuild = Set.fromList ["main", "master", "staging", "develop", "trunk"]
