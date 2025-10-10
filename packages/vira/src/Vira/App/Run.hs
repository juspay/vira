module Vira.App.Run (
  runVira,
) where

import Control.Concurrent.STM qualified as STM
import Control.Exception (bracket)
import Data.Acid (AcidState)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Version (showVersion)
import Effectful (runEff)
import Main.Utf8 qualified as Utf8
import Paths_vira qualified
import Relude hiding (atomically, newTVarIO)
import Vira.App qualified as App
import Vira.App.CLI (CLISettings (..), Command (..), GlobalSettings (..), WebSettings (..))
import Vira.App.CLI qualified as CLI
import Vira.App.InstanceInfo (getInstanceInfo)
import Vira.App.LinkTo.Resolve (linkTo)
import Vira.App.Server qualified as Server
import Vira.Refresh.Daemon (mkRefreshDaemon, startRefreshDaemon)
import Vira.State.Acid (ViraState)
import Vira.State.Core (closeViraState, openViraState, viraDbVersion)
import Vira.State.JSON (getExportData, importViraState)
import Vira.Supervisor.Core qualified as Supervisor
import Vira.Tool.Core qualified as Tool
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
    runAppWith CLISettings {globalSettings, command} = do
      case command of
        WebCommand webSettings -> runWebServer globalSettings webSettings
        ExportCommand -> runExport globalSettings
        ImportCommand -> runImport globalSettings
        InfoCommand -> runInfo

    runWebServer :: GlobalSettings -> WebSettings -> IO ()
    runWebServer globalSettings webSettings = do
      withViraState globalSettings $ \acid -> do
        -- Import data if specified
        whenJust (importFile webSettings) $ \filePath -> do
          importFromFileOrStdin acid (Just filePath)

        instanceInfo <- getInstanceInfo
        supervisor <- Supervisor.newSupervisor (CLI.stateDir globalSettings)
        -- Initialize broadcast channel for state update tracking
        stateUpdateBuffer <- STM.atomically STM.newBroadcastTChan
        -- Create TVar with all tools data for caching
        toolsVar <- runEff Tool.newToolsTVar

        -- Initialize refresh daemon TVar
        refreshDaemonTVar <- mkRefreshDaemon

        -- Create appState with daemon TVar
        let appState = App.AppState {App.instanceInfo = instanceInfo, App.linkTo = linkTo, App.acid = acid, App.supervisor = supervisor, App.stateUpdated = stateUpdateBuffer, App.tools = toolsVar, App.refreshDaemon = refreshDaemonTVar}

        let appServer = do
              -- Start refresh daemon (will create state and populate the TVar)
              startRefreshDaemon globalSettings (CLI.refreshInterval globalSettings)
              -- Run the server
              Server.runServer globalSettings webSettings
        App.runApp globalSettings appState appServer

    runExport :: GlobalSettings -> IO ()
    runExport globalSettings = do
      withViraState globalSettings $ \acid -> do
        exportData <- getExportData acid
        LBS.putStr $ encode exportData

    runImport :: GlobalSettings -> IO ()
    runImport globalSettings = do
      withViraState globalSettings $ \acid -> do
        importFromFileOrStdin acid Nothing

    runInfo :: IO ()
    runInfo = do
      let viraVersion = showVersion Paths_vira.version
      putTextLn $ "Vira version: " <> toText viraVersion
      putTextLn $ "Schema version: " <> show viraDbVersion

    importFromFileOrStdin :: AcidState ViraState -> Maybe FilePath -> IO ()
    importFromFileOrStdin acid mFilePath = do
      jsonData <- maybe LBS.getContents readFileLBS mFilePath
      result <- importViraState acid jsonData
      case result of
        Left err -> do
          putTextLn $ "Import failed: " <> toText err
          exitFailure
        Right () -> do
          putTextLn $ "Imported from: " <> maybe "stdin" toText mFilePath

    withViraState globalSettings action = do
      bracket (openViraState (stateDir globalSettings) (autoResetState globalSettings)) closeViraState action
