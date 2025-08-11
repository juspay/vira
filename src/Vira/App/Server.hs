{-# LANGUAGE OverloadedRecordDot #-}

module Vira.App.Server (
  runServer,
) where

import Effectful (Eff)
import Effectful.Reader.Dynamic qualified as Reader
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Static (
  addBase,
  noDots,
  staticPolicy,
  (>->),
 )
import Paths_vira qualified
import Servant.Server.Generic (genericServe)
import Vira.App (AppStack, CLISettings (..))
import Vira.App.Logging
import Vira.Lib.TLS (TLSConfig (..), startWarpServer)
import Vira.Page.IndexPage qualified as IndexPage

-- | Run the Vira server with the given CLI settings
runServer :: (HasCallStack) => CLISettings -> Eff AppStack ()
runServer cliSettings = do
  log Info $ "Launching at " <> buildUrl cliSettings
  log Debug $ "CLI settings: " <> show cliSettings
  app <- buildApplication
  liftIO $ startWarpServer warpSettings cliSettings.tlsConfig app
  where
    buildApplication = do
      servantApp <- genericServe . IndexPage.handlers <$> Reader.ask
      middleware <- staticMiddleware
      pure $ middleware servantApp

    buildUrl settings = protocol <> "://" <> settings.host <> ":" <> show settings.port
      where
        protocol = case settings.tlsConfig of
          TLSDisabled -> "http"
          _ -> "https"

    staticMiddleware = do
      staticDir <- liftIO Paths_vira.getDataDir
      pure $ staticPolicy $ noDots >-> addBase staticDir

    warpSettings =
      Warp.defaultSettings
        & Warp.setHost (fromString $ toString cliSettings.host)
        & Warp.setPort cliSettings.port
