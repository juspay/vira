{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.App.CLI where

import OptEnvConf
import Prelude hiding (reader)

-- | CLI settings
data Settings = Settings
  { -- Minimum logging level
    logLevel :: String -- TODO: Use `Colog.Core.Severity`
    -- The port to bind the HTTP server to
  , port :: Int
  , -- Path to vira db
    dbPath :: FilePath
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
    pure Settings {..}
