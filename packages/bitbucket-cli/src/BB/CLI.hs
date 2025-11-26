{- | CLI argument parsing for bb

Uses optparse-applicative for command-line parsing.
-}
module BB.CLI (
  CLISettings (..),
  Command (..),
  AuthArgs (..),
  SignoffArgs (..),
  StatusArgs (..),
  parseCLI,
) where

import Bitbucket.API.V1.BuildStatus (BuildState (..))
import Data.Char (toLower)
import Options.Applicative (
  Parser,
  ParserInfo,
  execParser,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  progDesc,
  showDefault,
  strOption,
  switch,
  value,
 )
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
  = SignoffCommand SignoffArgs
  | AuthCommand AuthArgs
  | StatusCommand StatusArgs
  deriving stock (Show, Eq)

-- | Arguments for auth command
newtype AuthArgs = AuthArgs
  { baseUrl :: Text
  -- ^ Bitbucket base URL
  }
  deriving stock (Show, Eq)

-- | Arguments for status command
newtype StatusArgs = StatusArgs
  { jsonOutput :: Bool
  -- ^ Output in JSON format
  }
  deriving stock (Show, Eq)

-- | Arguments for signoff command
data SignoffArgs = SignoffArgs
  { state :: BuildState
  -- ^ Build state (successful, failed, inprogress)
  , key :: Text
  -- ^ Unique identifier for this build
  , name :: Text
  -- ^ Display name for the build
  , url :: Text
  -- ^ URL to view build details
  , description :: Text
  -- ^ Description of the build status
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
  where
    short = OA.short

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
              (AuthCommand <$> authArgsParser)
              (progDesc "Configure Bitbucket authentication")
          )
        <> OA.command
          "status"
          ( info
              (StatusCommand <$> statusArgsParser)
              (progDesc "Check authentication status")
          )
    )

-- | Parser for auth arguments
authArgsParser :: Parser AuthArgs
authArgsParser =
  AuthArgs
    <$> strOption
      ( long "url"
          <> metavar "URL"
          <> help "Bitbucket base URL (e.g., https://bitbucket.example.com)"
      )

-- | Parser for status arguments
statusArgsParser :: Parser StatusArgs
statusArgsParser =
  StatusArgs
    <$> switch
      ( long "json"
          <> help "Output in JSON format"
      )

-- | Parser for signoff arguments
signoffArgsParser :: Parser SignoffArgs
signoffArgsParser =
  SignoffArgs
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
