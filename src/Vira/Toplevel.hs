{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.Toplevel (
  runVira,
) where

import Control.Exception (bracket)
import Data.Maybe (fromJust)
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

    -- Vira application for given `Settings`
    app :: (HasCallStack) => Settings -> Eff AppStack ()
    app settings = do
      -- Check TLS configuration from CLI arguments
      let tlsEnabled = isJust settings.tlsCert && isJust settings.tlsKey
      let protocol = if tlsEnabled then "https" else "http"
      log Info $ "Launching vira (" <> settings.instanceName <> ") at " <> protocol <> "://" <> settings.host <> ":" <> show settings.port
      log Debug $ "Settings: " <> show settings

      when tlsEnabled $ do
        log Info "TLS certificates provided - enabling HTTPS with HTTP/2 support"
        log Debug $ "TLS certificate: " <> toText (fromMaybe "" settings.tlsCert)
        log Debug $ "TLS key: " <> toText (fromMaybe "" settings.tlsKey)
      unless tlsEnabled $ do
        log Info "No TLS certificates provided - running HTTP only"
        log Info "Use --tls-cert and --tls-key to enable HTTPS with HTTP/2 support"

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

      if tlsEnabled
        then do
          let tlsCertPath = fromJust settings.tlsCert
          let tlsKeyPath = fromJust settings.tlsKey
          let tlsSettings = WarpTLS.tlsSettings tlsCertPath tlsKeyPath
          liftIO $ WarpTLS.runTLS tlsSettings warpSettings $ staticMiddleware servantApp
        else do
          liftIO $ Warp.runSettings warpSettings $ staticMiddleware servantApp
