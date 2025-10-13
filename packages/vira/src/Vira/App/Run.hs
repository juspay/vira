{-# LANGUAGE OverloadedRecordDot #-}

module Vira.App.Run (
  runVira,
) where

import Control.Concurrent.STM (newBroadcastTChan)
import Control.Exception (bracket)
import Data.Acid (AcidState)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Version (showVersion)
import Effectful (runEff)
import Main.Utf8 qualified as Utf8
import Paths_vira qualified
import Vira.App qualified as App
import Vira.App.CLI (CLISettings (..), Command (..), GlobalSettings (..), WebSettings (..))
import Vira.App.CLI qualified as CLI
import Vira.App.InstanceInfo (getInstanceInfo)
import Vira.Refresh.Daemon qualified as Daemon
import Vira.Refresh.Type qualified as Refresh
import Vira.State.Core (closeViraState, openViraState, startPeriodicArchival, viraDbVersion)
import Vira.State.JSON (getExportData, importViraState)
import Vira.State.Type (ViraState)
import Vira.Supervisor.Core qualified as Supervisor
import Vira.Tool.Core qualified as Tool
import Vira.Web.LinkTo.Resolve (linkTo)
import Vira.Web.Server qualified as Server
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
        supervisor <- Supervisor.newSupervisor (stateDir globalSettings)
        -- Initialize broadcast channel for state update tracking
        stateUpdateBuffer <- atomically newBroadcastTChan
        -- Create TVar with all tools data for caching
        toolsVar <- runEff Tool.newToolsTVar
        -- Initialize refresh state
        refreshState <- Refresh.newRefreshState
        let viraRuntimeState = App.ViraRuntimeState {App.instanceInfo = instanceInfo, App.linkTo = linkTo, App.acid = acid, App.supervisor = supervisor, App.updateBroadcast = stateUpdateBuffer, App.tools = toolsVar, App.refreshState = refreshState}
            appServer = do
              liftIO $ startPeriodicArchival acid
              Daemon.startRefreshDaemon
              Server.runServer globalSettings webSettings
        App.runApp globalSettings viraRuntimeState appServer

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

    withViraState gs =
      openViraState gs.stateDir gs.autoResetState
        `bracket` closeViraState
