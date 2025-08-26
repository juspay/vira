{-# LANGUAGE OverloadedRecordDot #-}

module Vira.App.Server (
  runServer,
) where

import Colog (Message, Severity (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.FileSystem (FileSystem, doesDirectoryExist)
import Effectful.Reader.Dynamic qualified as Reader
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
import Vira.App (AppStack, CLISettings (..))
import Vira.Lib.Logging
import Vira.Page.IndexPage qualified as IndexPage

-- | Run the Vira server with the given CLI settings
runServer :: (HasCallStack) => CLISettings -> Eff AppStack ()
runServer cliSettings = do
  log Info $ "Launching at " <> buildUrl cliSettings
  log Debug $ "CLI settings: " <> show cliSettings
  app <- buildApplication
  liftIO $ startWarpServer warpSettings cliSettings.stateDir cliSettings.tlsConfig app
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
      staticDir <- getDataDirMultiHome
      putStrLn staticDir
      pure $ staticPolicy $ noDots >-> addBase staticDir

    warpSettings =
      Warp.defaultSettings
        & Warp.setHost (fromString $ toString cliSettings.host)
        & Warp.setPort cliSettings.port

-- Like Paths_vira.getDataDir but GHC multi-home friendly
getDataDirMultiHome :: (IOE :> es, FileSystem :> es, Log Message :> es) => Eff es FilePath
getDataDirMultiHome = do
  p <- liftIO Paths_vira.getDataDir
  doesDirectoryExist p >>= \case
    True -> pure p
    False -> do
      -- We expect this to happen because, sadly, cabal doesn't work with GHC multi-home units setup.
      -- Only allow falling back to ./static when running inside a nix shell.
      inNixShell >>= \case
        True -> do
          log Warning $ "Data dir not found at " <> toText p <> ", falling back to local ./static path (IN_NIX_SHELL set)"
          pure "static"
        False -> do
          log Error $ "Data dir not found at " <> toText p
          die "Data directory not found"
  where
    inNixShell :: (IOE :> es) => Eff es Bool
    inNixShell = do
      isJust <$> lookupEnv "IN_NIX_SHELL"
