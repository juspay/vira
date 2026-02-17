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
import Network.HTTP.Types (status404, status503)
import Network.Wai (Application, Middleware, Request (pathInfo), responseLBS, responseStatus)
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
import System.Nix.Cache.Server qualified as Cache
import System.Nix.Flake.Develop qualified as Nix
import Vira.App (AppStack, ViraRuntimeState (..))
import Vira.App.CLI (GHAppAuthSettings (..), GlobalSettings (..), WebSettings (..))
import Vira.Effect.GitHub (newAppAuth)
import Vira.Lib.Crypto (readRsaPem)
import Vira.Web.Pages.IndexPage qualified as IndexPage
import Vira.Web.Pages.NotFoundPage qualified as NotFoundPage
import Vira.Webhook.GitHub qualified as WebhookGitHub

-- | Run the Vira server with the given 'GlobalSettings' and 'WebSettings'
runServer :: (HasCallStack) => GlobalSettings -> WebSettings -> Application -> Eff AppStack ()
runServer globalSettings webSettings cacheApp = do
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

      -- Start: GitHub Webhook Middleware init
      ghwebhookMW <- case webSettings.ghAppAuthSettings of
        Just settings -> do
          rsaPem <- liftIO $ readFileBS settings.privateKeyPath
          privateKey <- readRsaPem rsaPem
          appAuth <- liftIO $ newAppAuth privateKey settings.appId
          pure $ WebhookGitHub.webhookMiddleware globalSettings viraRuntimeState webSettings appAuth
        Nothing -> pure $ \app req sendResponse ->
          case pathInfo req of
            ("webhook" : "github" : _) ->
              sendResponse $
                responseLBS
                  status503
                  [("Content-Type", "text/plain")]
                  "GitHub webhook not configured. Set --github-app-id and --github-app-private-key to enable."
            _ -> app req sendResponse
      -- End: GitHub Webhook Middleware init

      let middlewares =
            [ -- 404 handler (innermost, applied last)
              notFoundMiddleware globalSettings viraRuntimeState webSettings
            , -- Middleware to serve static files
              staticPolicy $ noDots >-> addBase staticDir
            , -- Cache server middleware
              Cache.cacheMiddleware "cache" cacheApp
            , -- GitHub webhook sub-app (initializes its own context)
              ghwebhookMW
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
        & Warp.setTimeout 600 -- 10 minutes (for long builds with SSE heartbeats)

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

-- | WAI 'Middleware' to handle 404 errors with custom page
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
