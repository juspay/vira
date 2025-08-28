module Vira.App.Run (
  runVira,
) where

import Control.Concurrent.STM qualified as STM
import Control.Exception (bracket)
import Main.Utf8 qualified as Utf8
import Vira.App (CLISettings (..))
import Vira.App qualified as App
import Vira.App.CLI qualified as CLI
import Vira.App.LinkTo.Resolve (linkTo)
import Vira.App.Server qualified as Server
import Vira.Event.Logger qualified as Logger
import Vira.State.Core (closeViraState, openViraState)
import Vira.Supervisor.Core qualified as Supervisor
import Prelude hiding (Reader, ask, runReader)

-- | Run the Vira application
runVira :: IO ()
runVira = do
  Utf8.withUtf8 $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    runAppWith =<< CLI.parseCLI
  where
    runAppWith :: CLISettings -> IO ()
    runAppWith cliSettings = do
      bracket (openViraState (stateDir cliSettings)) closeViraState $ \acid -> do
        supervisor <- Supervisor.newSupervisor (stateDir cliSettings)
        eventSubscribers <- STM.newTVarIO [] -- Initialize empty subscriber list
        let appState = App.AppState {App.linkTo = linkTo, App.acid = acid, App.supervisor = supervisor, App.cliSettings = cliSettings, App.eventSubscribers = eventSubscribers}
            appServer = do
              Logger.startEventLogger -- Start event logging in background
              Server.runServer cliSettings
        App.runApp appState appServer
