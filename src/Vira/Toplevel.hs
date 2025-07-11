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
import Vira.Lib.TLS (TLSConfig (..), ensureTLSSettings)
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
    appIO =<< CLI.parseCLI
  where
    -- Like `app` but in `IO`
    appIO :: Settings -> IO ()
    appIO settings = do
      let repos = settings.repo.cloneUrls
      bracket (openViraState repos) closeViraState $ \acid -> do
        supervisor <- Vira.Supervisor.newSupervisor
        let st = App.AppState {linkTo = linkTo, ..}
        App.runApp st $ app settings

-- | Vira application for given `Settings`
app :: (HasCallStack) => Settings -> Eff AppStack ()
app settings = do
  -- Check TLS configuration from CLI arguments
  let protocol = case settings.tlsConfig of
        TLSDisabled -> "http"
        TLSAuto -> "https"
        TLSExplicit _ _ -> "https"
  log Info $ "Launching vira (" <> settings.instanceName <> ") at " <> protocol <> "://" <> settings.host <> ":" <> show settings.port
  log Debug $ "Settings: " <> show settings

  case settings.tlsConfig of
    TLSExplicit certPath keyPath -> do
      log Info "TLS certificates configured - enabling HTTPS with HTTP/2 support"
      log Debug $ "TLS certificate: " <> toText certPath
      log Debug $ "TLS key: " <> toText keyPath
    TLSAuto -> do
      log Info "HTTPS enabled with auto-generated certificates - enabling HTTP/2 support"
    TLSDisabled -> do
      log Info "HTTPS explicitly disabled - running HTTP only"
      log Info "HTTPS is enabled by default. Use --tls-cert/--tls-key for custom certificates"

  staticDir <- liftIO Paths_vira.getDataDir
  log Debug $ "Serving static files from: " <> show staticDir
  let staticMiddleware = staticPolicy $ noDots >-> addBase staticDir
  cfg <- ask
  let servantApp = genericServe $ Routes.handlers cfg
  let host = fromString $ toString settings.host
  let warpSettings =
        Warp.defaultSettings
          & Warp.setHost host
          & Warp.setPort settings.port

  case settings.tlsConfig of
    TLSExplicit certPath keyPath -> do
      let tlsSettings = WarpTLS.tlsSettings certPath keyPath
      log Info $ "Starting HTTPS server with HTTP/2 support on " <> settings.host <> ":" <> show settings.port
      log Info "Note: Self-signed certificates will show browser warnings - this is normal for development"
      liftIO $ WarpTLS.runTLS tlsSettings warpSettings $ staticMiddleware servantApp
    TLSAuto -> do
      -- Generate certificates just before starting the HTTPS server
      tlsSettings <- liftIO $ ensureTLSSettings "./state/tls" settings.host
      log Info $ "Starting HTTPS server with HTTP/2 support on " <> settings.host <> ":" <> show settings.port
      log Info "Note: Self-signed certificates will show browser warnings - this is normal for development"
      liftIO $ WarpTLS.runTLS tlsSettings warpSettings $ staticMiddleware servantApp
    TLSDisabled -> do
      log Info $ "Starting HTTP server on " <> settings.host <> ":" <> show settings.port
      liftIO $ Warp.runSettings warpSettings $ staticMiddleware servantApp
