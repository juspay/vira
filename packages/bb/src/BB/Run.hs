{-# LANGUAGE OverloadedRecordDot #-}

-- | Command execution for bb CLI
module BB.Run (
  runBB,
) where

import BB.CLI.Core (AuthCommand (..), CLISettings, Command (..))
import BB.CLI.Core qualified as CLI
import BB.Command.Auth.Login qualified as Login
import BB.Command.Auth.Status qualified as Status
import BB.Command.Signoff qualified as Signoff
import Colog (Severity (..))
import Effectful (runEff)
import Effectful.Colog.Simple (LogContext (..), log, runLogActionStdout)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Process (runProcess)
import Effectful.Reader.Static qualified as ER

-- | Main entry point for bb CLI
runBB :: IO ()
runBB = do
  settings <- CLI.parseCLI
  runCommand settings

-- | Run a command
runCommand :: CLISettings -> IO ()
runCommand settings =
  runEff . runLogActionStdout Info . ER.runReader (LogContext []) . runProcess $ do
    result <- runErrorNoCallStack @Text $ do
      case settings.command of
        SignoffCommand args -> Signoff.runSignoff settings.force args
        AuthCommand authCmd -> case authCmd of
          LoginCommand {baseUrl} -> Login.runLogin baseUrl
          StatusCommand {jsonOutput} -> Status.runStatus jsonOutput
    either (\err -> log Error err >> liftIO exitFailure) pure result
