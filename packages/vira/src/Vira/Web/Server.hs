{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Web.Server (
  runServer,
) where

import Colog.Message (RichMessage)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple
import Effectful.FileSystem (FileSystem, doesDirectoryExist)
import Effectful.Reader.Dynamic qualified as Reader
import Effectful.Reader.Static qualified as ER
import Network.HTTP.Types (status404)
import Network.Wai (Middleware, responseLBS, responseStatus)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS.Simple (TLSConfig (..), startWarpServer)
import Network.Wai.Middleware.Static (
  addBase,
  noDots,
  staticPolicy,
  (>->),
 )
import Paths_vira qualified
import Servant.Server.Generic (genericServe)
import System.Nix.Flake.Develop qualified as Nix
import Vira.App (AppStack)
import Vira.App.CLI (GlobalSettings (..), WebSettings (..))
import Vira.App.Type (ViraRuntimeState)
import Vira.Web.Pages.IndexPage qualified as IndexPage
import Vira.Web.Pages.NotFoundPage qualified as NotFoundPage

-- | Run the Vira server with the given settings
runServer :: (HasCallStack) => GlobalSettings -> WebSettings -> Eff AppStack ()
runServer globalSettings webSettings = do
  log Info $ "Launching at " <> buildUrl webSettings
  log Debug $ "Global settings: " <> show globalSettings
  log Debug $ "Web settings: " <> show webSettings
  app <- buildApplication
  liftIO $ startWarpServer (warpSettings webSettings) globalSettings.stateDir webSettings.tlsConfig app
  where
    buildApplication = do
      viraRuntimeState <- Reader.ask @ViraRuntimeState
      let servantApp = genericServe $ IndexPage.handlers globalSettings viraRuntimeState webSettings
      staticDir <- getDataDirMultiHome
      log Debug $ "Static dir = " <> toText staticDir
      let middlewares =
            [ -- Middleware to serve static files
              staticPolicy $ noDots >-> addBase staticDir
            , -- 404 handler
              notFoundMiddleware globalSettings viraRuntimeState webSettings
            ]
          app = foldl' (&) servantApp middlewares
      pure app

    buildUrl ws = protocol <> "://" <> ws.host <> ":" <> show ws.port
      where
        protocol = case ws.tlsConfig of
          TLSDisabled -> "http"
          _ -> "https"

    warpSettings ws =
      Warp.defaultSettings
        & Warp.setHost (fromString $ toString ws.host)
        & Warp.setPort ws.port

-- Like Paths_vira.getDataDir but GHC multi-home friendly
getDataDirMultiHome :: (IOE :> es, FileSystem :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es) => Eff es FilePath
getDataDirMultiHome = do
  p <- liftIO Paths_vira.getDataDir
  doesDirectoryExist p >>= \case
    True -> pure p
    False -> do
      -- We expect this to happen because, sadly, cabal doesn't work with GHC multi-home units setup.
      -- Only allow falling back to ./static when running inside a nix shell.
      liftIO Nix.inNixShell >>= \case
        True -> do
          log Warning $ "Data dir not found at " <> toText p <> ", falling back to local ./static path (IN_NIX_SHELL set)"
          pure "static"
        False -> do
          log Error $ "Data dir not found at " <> toText p
          die "Data directory not found"

-- | WAI middleware to handle 404 errors with custom page
notFoundMiddleware :: GlobalSettings -> ViraRuntimeState -> WebSettings -> Middleware
notFoundMiddleware globalSettings viraRuntimeState webSettings app req respond = do
  app req $ \res -> do
    -- Check if the response is a 404
    if responseStatus res == status404
      then do
        html404 <- NotFoundPage.complete404Page globalSettings viraRuntimeState webSettings
        respond $
          responseLBS status404 [("Content-Type", "text/html; charset=utf-8")] $
            encodeUtf8 html404
      else respond res
