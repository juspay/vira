{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (callProcess)
import System.Which (staticWhich)
import Vira.App (AppStack, Settings (..))
import Vira.App qualified as App
import Vira.App.CLI qualified as CLI
import Vira.App.LinkTo.Resolve (linkTo)
import Vira.App.Logging
import Vira.Routes qualified as Routes
import Vira.State.Core (closeViraState, openViraState)
import Vira.Supervisor qualified
import Prelude hiding (Reader, ask, runReader)

{- | Path to the `openssl` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
opensslBin :: FilePath
opensslBin = $(staticWhich "openssl")

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
      -- Generate TLS certificates automatically if needed
      (certPath, keyPath) <- generateTLSCertificatesIfNeeded settings
      let updatedSettings = settings {tlsCert = certPath, tlsKey = keyPath}

      let repos = updatedSettings.repo.cloneUrls
      bracket (openViraState repos) closeViraState $ \acid -> do
        supervisor <- Vira.Supervisor.newSupervisor
        let st = App.AppState {linkTo = linkTo, ..}
        App.runApp st $ app updatedSettings

-- | Generate TLS certificates automatically if they don't exist
generateTLSCertificatesIfNeeded :: Settings -> IO (Maybe FilePath, Maybe FilePath)
generateTLSCertificatesIfNeeded settings = do
  -- If user provided explicit TLS paths, use those
  case (settings.tlsCert, settings.tlsKey) of
    (Just cert, Just key) -> pure (Just cert, Just key)
    _ -> do
      -- Auto-generate certificates in ./state/tls/
      let certDir = "./state/tls"
      let certPath = certDir <> "/server.crt"
      let keyPath = certDir <> "/server.key"

      certExists <- doesFileExist certPath
      keyExists <- doesFileExist keyPath

      if certExists && keyExists
        then do
          putTextLn "Using existing TLS certificates from ./state/tls/"
          pure (Just certPath, Just keyPath)
        else do
          putTextLn "Generating TLS certificates for HTTPS support..."
          createDirectoryIfMissing True certDir
          generateCertificates certDir settings.host
          pure (Just certPath, Just keyPath)

-- | Generate self-signed certificates with proper SAN for local network access
generateCertificates :: FilePath -> Text -> IO ()
generateCertificates certDir hostArg = do
  let certPath = certDir <> "/server.crt"
  let keyPath = certDir <> "/server.key"

  -- Generate private key
  callProcess opensslBin ["genrsa", "-out", keyPath, "2048"]

  -- Create OpenSSL config with comprehensive SAN list
  let opensslConfig =
        unlines
          [ "[req]"
          , "distinguished_name = req_distinguished_name"
          , "req_extensions = v3_req"
          , "prompt = no"
          , ""
          , "[req_distinguished_name]"
          , "C = US"
          , "ST = CA"
          , "L = San Francisco"
          , "O = Vira Development"
          , "OU = IT Department"
          , "CN = localhost"
          , ""
          , "[v3_req]"
          , "basicConstraints = CA:FALSE"
          , "keyUsage = critical, digitalSignature, keyEncipherment, keyAgreement"
          , "extendedKeyUsage = critical, serverAuth, clientAuth"
          , "subjectAltName = @alt_names"
          , ""
          , "[alt_names]"
          , "DNS.1 = localhost"
          , "DNS.2 = " <> hostArg
          , "IP.1 = 127.0.0.1"
          , "IP.2 = ::1"
          , "IP.3 = 0.0.0.0"
          , "IP.4 = 192.168.1.1"
          , "IP.5 = 192.168.1.100"
          , "IP.6 = 192.168.0.1"
          , "IP.7 = 192.168.0.100"
          , "IP.8 = 10.0.0.1"
          , "IP.9 = 10.0.0.100"
          , "IP.10 = 172.16.0.1"
          , "IP.11 = 172.16.0.100"
          ]

  let configPath = certDir <> "/openssl.conf"
  writeFileText configPath opensslConfig

  -- Generate self-signed certificate with longer validity
  callProcess
    opensslBin
    [ "req"
    , "-new"
    , "-x509"
    , "-key"
    , keyPath
    , "-out"
    , certPath
    , "-days"
    , "3650" -- 10 years for development
    , "-config"
    , configPath
    ]

  putTextLn "Generated TLS certificates:"
  putTextLn $ "  Certificate: " <> toText certPath
  putTextLn $ "  Private key: " <> toText keyPath
  putTextLn $ "  Valid for: localhost, 127.0.0.1, " <> hostArg <> ", and common local network IPs"

-- | Vira application for given `Settings`
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
      log Info $ "Starting HTTPS server with HTTP/2 support on " <> settings.host <> ":" <> show settings.port
      log Info "Note: Self-signed certificates will show browser warnings - this is normal for development"
      liftIO $ WarpTLS.runTLS tlsSettings warpSettings $ staticMiddleware servantApp
    else do
      log Info $ "Starting HTTP server on " <> settings.host <> ":" <> show settings.port
      liftIO $ Warp.runSettings warpSettings $ staticMiddleware servantApp
