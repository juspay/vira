{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.App.CLI where

import Data.Set qualified as Set
import OptEnvConf
import Prelude hiding (Reader, reader)

{- | CLI settings
TODO: Use Severity from co-log
-}
data Settings = Settings
  { logLevel :: String
  -- ^ Minimum logging level
  , port :: Int
  -- ^ The port to bind the HTTP server to
  , dbPath :: FilePath
  -- ^ Path to vira db
  , repos :: [Text]
  -- ^ Repositories (git clone URL) to watch and build
  , branchWhitelist :: Set Text
  -- ^ Limit to building these branches to build
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
    dbPath <-
      setting
        [ reader str
        , metavar "DB_PATH"
        , help "Path to vira db"
        , name "db-path"
        , value "vira.db"
        ]
    repos <-
      setting
        [ reader (commaSeparatedList $ fmap (toText @String) str)
        , metavar "REPOS"
        , help "Repositories to watch and build (comma-separated Git clone URLs)"
        , name "repos"
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
    pure Settings {..}

defaultRepos :: [Text]
defaultRepos =
  [ "https://github.com/srid/emanote.git"
  , "https://github.com/juspay/omnix.git"
  , "https://github.com/srid/haskell-flake.git"
  , "https://github.com/juspay/hyperswitch.git"
  , "https://github.com/juspay/superposition.git"
  , "https://github.com/juspay/services-flake.git"
  ]

defaultBranchesToBuild :: Set Text
defaultBranchesToBuild = Set.fromList ["main", "master", "staging", "develop", "trunk"]
