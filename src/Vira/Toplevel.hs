{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.Toplevel (
  runVira,
) where

import Control.Exception (bracket)
import Effectful (Eff)
import Effectful.Reader.Dynamic (ask)
import Main.Utf8 qualified as Utf8
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import Network.Wai.Middleware.Static (
  addBase,
  noDots,
  staticPolicy,
  (>->),
 )
import Paths_vira qualified
import Servant.Server.Generic (genericServe)
import Vira.App (AppStack, Settings (..))
import Vira.App qualified as App
import Vira.App.CLI qualified as CLI
import Vira.App.LinkTo.Resolve (linkTo)
import Vira.App.Logging
import Vira.Lib.TLS (TLSConfig (..), tlsConfigResolve)
import Vira.Routes qualified as Routes
import Vira.State.Core (closeViraState, openViraState)
import Vira.Supervisor qualified
import Prelude hiding (Reader, ask, runReader)

-- | Run the Vira application
runVira :: IO ()
runVira = do
  Utf8.withUtf8 $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    runAppIO =<< CLI.parseCLI
  where
    -- Like `runAppEff` but in `IO`
    runAppIO :: Settings -> IO ()
    runAppIO settings = do
      let repos = map (.repoInfo) settings.repo.repoSettings
      bracket (openViraState repos) closeViraState $ \acid -> do
        supervisor <- Vira.Supervisor.newSupervisor
        let st = App.AppState {linkTo = linkTo, ..}
        App.runApp st $ runAppEff settings

-- | Vira application for given `Settings`
runAppEff :: (HasCallStack) => Settings -> Eff AppStack ()
runAppEff settings = do
  let protocol = case settings.tlsConfig of
        TLSDisabled -> "http"
        _ -> "https"
  log Info $ "Launching vira (" <> settings.instanceName <> ") at " <> protocol <> "://" <> settings.host <> ":" <> show settings.port
  log Debug $ "Settings: " <> show settings

  staticDir <- liftIO Paths_vira.getDataDir
  let staticMiddleware = staticPolicy $ noDots >-> addBase staticDir
  servantApp <- genericServe . Routes.handlers <$> ask
  let host = fromString $ toString settings.host
  let warpSettings =
        Warp.defaultSettings
          & Warp.setHost host
          & Warp.setPort settings.port
  let app = staticMiddleware servantApp

  liftIO (tlsConfigResolve settings.tlsConfig) >>= \case
    Nothing -> do
      liftIO $ Warp.runSettings warpSettings app
    Just tlsSettings -> do
      liftIO $ WarpTLS.runTLS tlsSettings warpSettings app
