{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.App.Run (
  runVira,
) where

import Control.Exception (bracket)
import Data.Acid (AcidState)
import Data.Acid.Events qualified as Event
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Time (getCurrentTime)
import Data.Version (showVersion)
import Effectful (runEff)
import Effectful.Colog.Simple (runLogActionStdout)
import Effectful.Concurrent.Async (runConcurrent)
import Effectful.Environment (runEnvironment)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.FileSystem (runFileSystem)
import Effectful.Git (RepoName (..), getRemoteUrl)
import Effectful.Git.Command.Status (GitStatusPorcelain (..), gitStatusPorcelain)
import Effectful.Process (runProcess)
import Main.Utf8 qualified as Utf8
import Paths_vira qualified
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Exit (ExitCode (..))
import System.Nix.Cache.Keys qualified as CacheKeys
import System.Nix.Cache.Server qualified as Cache
import Vira.App qualified as App
import Vira.App.CLI (CLISettings (..), Command (..), GlobalSettings (..), WebSettings (..))
import Vira.App.CLI qualified as CLI
import Vira.App.InstanceInfo (getInstanceInfo)
import Vira.CI.AutoBuild qualified as AutoBuild
import Vira.CI.Cleanup.Daemon qualified as CleanupDaemon
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Pipeline qualified as Pipeline
import Vira.CI.Pipeline.Program qualified as Program
import Vira.CI.Worker qualified as Worker
import Vira.CI.Worker.Type qualified as Worker
import Vira.Environment.Tool.Core qualified as Tool
import Vira.Refresh.Daemon qualified as Daemon
import Vira.Refresh.Type qualified as Refresh
import Vira.State.Core (closeViraState, openViraState, startPeriodicArchival, viraDbVersion)
import Vira.State.JSON (getExportData, importViraState)
import Vira.State.Type (Repo (..), ViraState)
import Vira.Supervisor.Core qualified as Supervisor
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
        CICommand mDir -> runCI globalSettings mDir

    runWebServer :: GlobalSettings -> WebSettings -> IO ()
    runWebServer globalSettings webSettings = do
      withViraState globalSettings $ \acid -> do
        -- Import data if specified
        whenJust (importFile webSettings) $ \filePath -> do
          importFromFileOrStdin acid (Just filePath)

        startTime <- getCurrentTime
        instanceInfo <- getInstanceInfo
        supervisor <- Supervisor.newSupervisor (stateDir globalSettings)
        -- Initialize event bus for update tracking (SSE, subscriptions, debug log)
        eventBus <- Event.newEventBus
        -- Create TVar with all tools data for caching
        tools <- runEff $ runLogActionStdout (logLevel globalSettings) $ runProcess Tool.newToolsTVar
        -- Initialize refresh state
        refreshState <- Refresh.newRefreshState
        -- Initialize job worker state
        let maxConcurrent = webSettings.ciSettings.maxConcurrentBuilds
            autoBuildSettings = AutoBuild.AutoBuildSettings {autoBuildNewBranches = webSettings.ciSettings.autoBuildNewBranches}
        jobWorker <- liftIO $ Worker.newJobWorkerState maxConcurrent (logLevel globalSettings)
        -- Ensure cache keys exist and create cache application
        cacheKeys <- runEff . runLogActionStdout (logLevel globalSettings) $ do
          CacheKeys.ensureCacheKeys $ stateDir globalSettings <> "/cache-keys"
        let cachePublicKey = cacheKeys.publicKey
            viraRuntimeState = App.ViraRuntimeState {linkTo, ..}
            appServer = do
              startPeriodicArchival acid
              Daemon.startRefreshDaemon
              Worker.startJobWorkerDaemon
              AutoBuild.startAutoBuildDaemon autoBuildSettings
              CleanupDaemon.startCleanupDaemon webSettings.ciSettings.jobRetentionDays
              cacheApp <- liftIO $ Cache.makeCacheServer cacheKeys.secretKey
              Server.runServer globalSettings webSettings cacheApp
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

    runCI :: GlobalSettings -> Maybe FilePath -> IO ()
    runCI gs mDir = do
      dir <- maybe getCurrentDirectory makeAbsolute mDir
      result <- runCIEffects gs dir
      case result of
        Left err -> do
          putTextLn $ "CI pipeline failed: " <> show err
          exitFailure
        Right exitCode -> do
          case exitCode of
            ExitSuccess -> putTextLn "CI pipeline succeeded"
            ExitFailure code -> do
              putTextLn $ "CI pipeline failed with exit code: " <> show code
              exitWith exitCode

    runCIEffects :: GlobalSettings -> FilePath -> IO (Either Pipeline.PipelineError ExitCode)
    runCIEffects gs repoDir =
      runEff
        . runLogActionStdout gs.logLevel
        . runEnvironment
        . runFileSystem
        . runProcess
        . runConcurrent
        . runErrorNoCallStack
        $ do
          GitStatusPorcelain {branch} <- runErrorNoCallStack (gitStatusPorcelain repoDir) >>= either error pure
          -- Get remote URL for creating Repo
          remoteUrl <- runErrorNoCallStack (getRemoteUrl repoDir "origin") >>= either error pure
          let repo = Repo {name = RepoName "cli-repo", cloneUrl = remoteUrl, lastRefresh = Nothing}
          let ctx = ViraContext branch True
          tools <- Tool.getAllTools
          Pipeline.runPipeline (Pipeline.pipelineEnvFromCLI gs.logLevel tools ctx) (Program.pipelineProgram repo repoDir)
            $> ExitSuccess

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
