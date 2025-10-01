module Vira.App.Run (
  runVira,
) where

import Control.Concurrent.STM (newBroadcastTChan)
import Control.Exception (bracket)
import Data.Acid (AcidState)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Main.Utf8 qualified as Utf8
import Vira.App qualified as App
import Vira.App.CLI (CLISettings (..), Command (..), GlobalSettings (..), WebSettings (..))
import Vira.App.CLI qualified as CLI
import Vira.App.InstanceInfo (getInstanceInfo)
import Vira.App.LinkTo.Resolve (linkTo)
import Vira.App.Server qualified as Server
import Vira.State.Core (ViraState, closeViraState, openViraState)
import Vira.State.JSON (getExportData, importViraState)
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
    runAppWith CLISettings {globalSettings, command} = do
      case command of
        WebCommand webSettings -> runWebServer globalSettings webSettings
        ExportCommand -> runExport globalSettings
        ImportCommand -> runImport globalSettings

    runWebServer :: GlobalSettings -> WebSettings -> IO ()
    runWebServer globalSettings webSettings = do
      withViraState globalSettings $ \acid -> do
        -- Import data if specified
        whenJust (importFile webSettings) $ \filePath -> do
          importFromFileOrStdin acid (Just filePath)

        instanceInfo <- getInstanceInfo
        supervisor <- Supervisor.newSupervisor (stateDir globalSettings)
        -- Initialize broadcast channel for state update tracking
        stateUpdateBuffer <- atomically newBroadcastTChan
        let appState = App.AppState {App.instanceInfo = instanceInfo, App.linkTo = linkTo, App.acid = acid, App.supervisor = supervisor, App.stateUpdated = stateUpdateBuffer}
            appServer = Server.runServer globalSettings webSettings
        App.runApp appState appServer

    runExport :: GlobalSettings -> IO ()
    runExport globalSettings = do
      withViraState globalSettings $ \acid -> do
        exportData <- getExportData acid
        LBS.putStr $ encode exportData

    runImport :: GlobalSettings -> IO ()
    runImport globalSettings = do
      withViraState globalSettings $ \acid -> do
        importFromFileOrStdin acid Nothing

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
      bracket (openViraState (stateDir globalSettings)) closeViraState action
