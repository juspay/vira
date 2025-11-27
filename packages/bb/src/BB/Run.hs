{-# LANGUAGE OverloadedRecordDot #-}

-- | Command execution for bb CLI
module BB.Run (
  runBB,
) where

import BB.CLI (AuthCommand (..), CLISettings)
import BB.CLI qualified as CLI
import BB.Command.Auth.Login qualified as Login
import BB.Command.Auth.Status qualified as Status
import Colog (Severity (..))
import Effectful (runEff)
import Effectful.Colog.Simple (LogContext (..), log, runLogActionStdout)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static qualified as ER

-- | Main entry point for bb CLI
runBB :: IO ()
runBB = do
  settings <- CLI.parseCLI
  runCommand settings

-- | Run a command
runCommand :: CLISettings -> IO ()
runCommand settings =
  runEff . runLogActionStdout Info . ER.runReader (LogContext []) $ do
    result <- runErrorNoCallStack @Text $ do
      case settings.command of
        LoginCommand {baseUrl} -> Login.runLogin baseUrl
        StatusCommand {jsonOutput} -> Status.runStatus jsonOutput
    either (\err -> log Error err >> liftIO exitFailure) pure result
