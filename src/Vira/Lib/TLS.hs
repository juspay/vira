{-# LANGUAGE TemplateHaskell #-}

module Vira.Lib.TLS (
  generateTLSCertificatesIfNeeded,
  generateCertificates,
) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (callProcess)
import System.Which (staticWhich)

{- | Path to the `openssl` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
opensslBin :: FilePath
opensslBin = $(staticWhich "openssl")

-- | Generate TLS certificates automatically if they don't exist
generateTLSCertificatesIfNeeded ::
  -- | Explicit cert path (if provided)
  Maybe FilePath ->
  -- | Explicit key path (if provided)
  Maybe FilePath ->
  -- | Host for certificate SAN
  Text ->
  IO (FilePath, FilePath)
generateTLSCertificatesIfNeeded maybeCert maybeKey hostArg = do
  -- If user provided explicit TLS paths, use those
  case (maybeCert, maybeKey) of
    (Just cert, Just key) -> pure (cert, key)
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
          pure (certPath, keyPath)
        else do
          putTextLn "Generating TLS certificates for HTTPS support..."
          createDirectoryIfMissing True certDir
          generateCertificates certDir hostArg
          pure (certPath, keyPath)

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
