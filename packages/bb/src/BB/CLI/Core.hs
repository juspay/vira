{- | CLI argument parsing for bb

Uses optparse-applicative for command-line parsing.
-}
module BB.CLI.Core (
  CLISettings (..),
  Command (..),
  AuthCommand (..),
  parseCLI,
) where

import Bitbucket.API.V1.BuildStatus (BuildState (..), BuildStatus (..))
import Data.Char (toLower)
import Options.Applicative
import Options.Applicative qualified as OA

-- | Top-level CLI settings
data CLISettings = CLISettings
  { force :: Bool
  -- ^ Force operation (skip dirty check)
  , command :: Command
  }
  deriving stock (Show, Eq)

-- | Available commands
data Command
  = SignoffCommand BuildStatus
  | AuthCommand AuthCommand
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
cliSettingsParser =
  CLISettings
    <$> switch (long "force" <> short 'f' <> help "Force operation (skip dirty check)")
    <*> commandParser

-- | Parser for commands
commandParser :: Parser Command
commandParser =
  hsubparser
    ( OA.command
        "signoff"
        ( info
            (SignoffCommand <$> signoffArgsParser)
            (progDesc "Sign off on current commit by posting build status")
        )
        <> OA.command
          "auth"
          ( info
              (AuthCommand <$> authCommandParser)
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

-- | Parser for signoff arguments
signoffArgsParser :: Parser BuildStatus
signoffArgsParser =
  BuildStatus
    <$> stateParser
    <*> strOption
      ( long "key"
          <> metavar "KEY"
          <> help "Unique identifier for this build"
          <> value "bb-signoff"
          <> showDefault
      )
    <*> strOption
      ( long "name"
          <> metavar "NAME"
          <> help "Display name for the build"
          <> value "bb signoff"
          <> showDefault
      )
    <*> strOption
      ( long "url"
          <> metavar "URL"
          <> help "URL to view build details"
          <> value "https://vira.nixos.asia/"
          <> showDefault
      )
    <*> strOption
      ( long "description"
          <> metavar "DESC"
          <> help "Description of the build status"
          <> value "Signed off via bb CLI"
          <> showDefault
      )

-- | Parser for build state
stateParser :: Parser BuildState
stateParser =
  OA.option
    stateReader
    ( long "state"
        <> metavar "STATE"
        <> help "Build state: successful, failed, inprogress"
        <> value Successful
        <> showDefault
    )

-- | Read build state from string
stateReader :: OA.ReadM BuildState
stateReader = OA.eitherReader $ \s ->
  case fmap toLower s of
    "successful" -> Right Successful
    "failed" -> Right Failed
    "inprogress" -> Right InProgress
    _ -> Left $ "Invalid state: " <> s <> ". Must be one of: successful, failed, inprogress"
