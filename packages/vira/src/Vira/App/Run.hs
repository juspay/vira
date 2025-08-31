module Vira.App.Run (
  runVira,
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.CircularBuffer qualified as CB
import Control.Exception (bracket)
import Main.Utf8 qualified as Utf8
import Vira.App (CLISettings (..))
import Vira.App qualified as App
import Vira.App.CLI qualified as CLI
import Vira.App.LinkTo.Resolve (linkTo)
import Vira.App.Server qualified as Server
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
        -- Initialize circular buffer for state update tracking (buffer size of 5)
        stateUpdateBuffer <- Control.Concurrent.STM.atomically $ CB.new 5
        let appState = App.AppState {App.linkTo = linkTo, App.acid = acid, App.supervisor = supervisor, App.cliSettings = cliSettings, App.stateUpdated = stateUpdateBuffer}
            appServer = Server.runServer cliSettings
        App.runApp appState appServer
