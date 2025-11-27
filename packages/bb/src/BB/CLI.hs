{- | CLI argument parsing for bb

Uses optparse-applicative for command-line parsing.
-}
module BB.CLI (
  CLISettings (..),
  AuthCommand (..),
  parseCLI,
) where

import Options.Applicative
import Options.Applicative qualified as OA

-- | Top-level CLI settings (just auth commands)
newtype CLISettings = CLISettings
  { command :: AuthCommand
  }
  deriving stock (Show, Eq)

-- | Auth subcommands
data AuthCommand
  = LoginCommand
      { baseUrl :: Text
      -- ^ Bitbucket base URL
      }
  | StatusCommand
      { jsonOutput :: Bool
      -- ^ Output in JSON format
      }
  deriving stock (Show, Eq)

-- | Parse CLI arguments
parseCLI :: IO CLISettings
parseCLI = execParser parseCLISettings

-- | Parser info
parseCLISettings :: ParserInfo CLISettings
parseCLISettings =
  info
    (cliSettingsParser OA.<**> helper)
    (progDesc "Bitbucket CLI tool")

-- | Parser for CLI settings
cliSettingsParser :: Parser CLISettings
cliSettingsParser = CLISettings <$> commandParser

-- | Parser for commands (auth only now)
commandParser :: Parser AuthCommand
commandParser =
  hsubparser
    ( OA.command
        "auth"
        ( info
            authCommandParser
            (progDesc "Configure Bitbucket authentication")
        )
    )

-- | Parser for auth subcommands
authCommandParser :: Parser AuthCommand
authCommandParser =
  hsubparser
    ( OA.command
        "login"
        ( info
            (LoginCommand <$> loginArgsParser)
            (progDesc "Authenticate to Bitbucket server")
        )
        <> OA.command
          "status"
          ( info
              (StatusCommand <$> statusArgsParser)
              (progDesc "Check authentication status")
          )
    )

-- | Parser for login arguments
loginArgsParser :: Parser Text
loginArgsParser =
  strArgument
    ( metavar "URL"
        <> help "Bitbucket base URL (e.g., https://bitbucket.example.com or bitbucket.example.com)"
    )

-- | Parser for status arguments
statusArgsParser :: Parser Bool
statusArgsParser =
  switch
    ( long "json"
        <> help "Output in JSON format"
    )
