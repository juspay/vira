{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Handler.WarpTLS.Simple (
  -- * Type
  TLSConfig (..),

  -- * Function
  tlsConfigResolve,
  startWarpServer,

  -- * CLI parser
  tlsConfigParser,
) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import Network.Wai.Handler.WarpTLS.Internal qualified as WarpTLS
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Process (callProcess)
import System.Which (staticWhich)
import Text.Show (Show (..))

{- | Path to the `openssl` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
opensslBin :: FilePath
opensslBin = $(staticWhich "openssl")

-- | TLS configuration with HTTPS enabled by default
data TLSConfig
  = -- | No TLS - run HTTP only (explicit)
    TLSDisabled
  | -- | TLS with auto-generated certificates (default)
    TLSAuto
  | -- | TLS with user-provided certificate and key files
    TLSExplicit WarpTLS.TLSSettings

instance Show TLSConfig where
  show = \case
    TLSDisabled -> "TLSDisabled"
    TLSAuto -> "TLSAuto"
    TLSExplicit tlsSettings -> "TLSExplicit (user-provided certificates): " <> Text.Show.show (WarpTLS.getCertSettings tlsSettings)

-- | Parser for TLS configuration with HTTPS enabled by default
tlsConfigParser :: Parser TLSConfig
tlsConfigParser =
  noHttpsMode <|> tlsExplicitMode <|> defaultMode
  where
    noHttpsMode =
      flag'
        TLSDisabled
        ( long "no-https"
            <> help "Disable HTTPS and run HTTP server only"
        )

    tlsExplicitMode =
      fmap TLSExplicit . WarpTLS.tlsSettings
        <$> strOption
          ( long "tls-cert"
              <> metavar "TLS_CERT"
              <> help "Path to TLS certificate file (requires --tls-key)"
          )
        <*> strOption
          ( long "tls-key"
              <> metavar "TLS_KEY"
              <> help "Path to TLS private key file (requires --tls-cert)"
          )

    -- Default to auto-generation (HTTPS enabled by default)
    defaultMode = pure TLSAuto

tlsConfigResolve :: FilePath -> TLSConfig -> IO (Maybe WarpTLS.TLSSettings)
tlsConfigResolve stateDir = \case
  TLSDisabled -> pure Nothing
  TLSAuto -> Just <$> ensureTLSSettings (stateDir </> "tls") "localhost"
  TLSExplicit tlsSettings -> pure (Just tlsSettings)

{- | Ensure TLS certificates exist for auto-generation mode
Returns TLSSettings configured with the certificate and key file paths
-}
ensureTLSSettings :: FilePath -> Text -> IO WarpTLS.TLSSettings
ensureTLSSettings certDir hostArg = do
  let (certPath, keyPath) = certPaths certDir

  certExists <- doesFileExist certPath
  keyExists <- doesFileExist keyPath

  if certExists && keyExists
    then do
      putTextLn $ "Using existing TLS certificates from " <> toText certDir <> "/"
    else do
      putTextLn "Generating TLS certificates for HTTPS support..."
      createDirectoryIfMissing True certDir
      generateCertificates certDir hostArg

  pure $ WarpTLS.tlsSettings certPath keyPath

-- | Helper function to construct certificate and key file paths from a directory
certPaths :: FilePath -> (FilePath, FilePath)
certPaths certDir =
  let certPath = certDir <> "/server.crt"
      keyPath = certDir <> "/server.key"
   in (certPath, keyPath)

-- | High-level certificate request configuration
data CertificateRequest = CertificateRequest
  { certSubject :: CertSubject
  , certValidityDays :: Int
  , certSANHosts :: [Text] -- Additional SAN hostnames
  , certSANIPs :: [Text] -- Additional SAN IP addresses
  }
  deriving stock (Show)

-- | Certificate subject information for self-signed certificates
data CertSubject = CertSubject
  { certCountry :: Text
  , certState :: Text
  , certLocality :: Text
  , certOrganization :: Text
  , certOrganizationalUnit :: Text
  , certCommonName :: Text
  }
  deriving stock (Show)

-- | Default certificate subject for development
defaultCertSubject :: CertSubject
defaultCertSubject =
  CertSubject
    { certCountry = "US"
    , certState = "CA"
    , certLocality = "San Francisco"
    , certOrganization = "Vira Development"
    , certOrganizationalUnit = "IT Department"
    , certCommonName = "localhost"
    }

-- | Default certificate request for local development
defaultCertRequest :: Text -> CertificateRequest
defaultCertRequest hostArg =
  CertificateRequest
    { certSubject = defaultCertSubject
    , certValidityDays = 3650 -- 10 years
    , certSANHosts = ["localhost", hostArg]
    , certSANIPs =
        [ "127.0.0.1"
        , "::1"
        , "0.0.0.0"
        , "192.168.1.1"
        , "192.168.1.100"
        , "192.168.0.1"
        , "192.168.0.100"
        , "10.0.0.1"
        , "10.0.0.100"
        , "172.16.0.1"
        , "172.16.0.100"
        ]
    }

-- | Generate a self-signed certificate using a certificate request
generateCertificateWithRequest :: FilePath -> CertificateRequest -> IO ()
generateCertificateWithRequest certDir request = do
  let (certPath, keyPath) = certPaths certDir

  -- Generate private key
  callProcess opensslBin ["genrsa", "-out", keyPath, "2048"]

  -- Create OpenSSL config
  let opensslConfig = generateOpenSSLConfig request
      configPath = certDir <> "/openssl.conf"

  writeFileText configPath opensslConfig

  -- Generate self-signed certificate
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
    , Prelude.show request.certValidityDays
    , "-config"
    , configPath
    ]

  putTextLn "Generated TLS certificates:"
  putTextLn $ "  Certificate: " <> toText certPath
  putTextLn $ "  Private key: " <> toText keyPath
  let hostList = intercalate ", " (map toString request.certSANHosts)
  putTextLn $ "  Valid for: " <> toText hostList <> " and common local network IPs"

-- | Generate OpenSSL configuration from a certificate request
generateOpenSSLConfig :: CertificateRequest -> Text
generateOpenSSLConfig request =
  let subject = request.certSubject
      dnsEntries =
        zipWith
          (\i host -> "DNS." <> Prelude.show i <> " = " <> host)
          [(1 :: Int) ..]
          request.certSANHosts
      ipEntries =
        zipWith
          (\i ip -> "IP." <> Prelude.show i <> " = " <> ip)
          [(length request.certSANHosts + 1 :: Int) ..]
          request.certSANIPs
      allSANEntries = dnsEntries <> ipEntries
      altNamesSection = unlines $ "[alt_names]" : allSANEntries
   in unlines
        [ "[req]"
        , "distinguished_name = req_distinguished_name"
        , "req_extensions = v3_req"
        , "prompt = no"
        , ""
        , "[req_distinguished_name]"
        , "C = " <> subject.certCountry
        , "ST = " <> subject.certState
        , "L = " <> subject.certLocality
        , "O = " <> subject.certOrganization
        , "OU = " <> subject.certOrganizationalUnit
        , "CN = " <> subject.certCommonName
        , ""
        , "[v3_req]"
        , "basicConstraints = CA:FALSE"
        , "keyUsage = critical, digitalSignature, keyEncipherment, keyAgreement"
        , "extendedKeyUsage = critical, serverAuth, clientAuth"
        , "subjectAltName = @alt_names"
        , ""
        , altNamesSection
        ]

-- | Generate self-signed certificates with proper SAN for local network access (simplified API)
generateCertificates :: FilePath -> Text -> IO ()
generateCertificates certDir hostArg =
  generateCertificateWithRequest certDir (defaultCertRequest hostArg)

-- | Start a Warp server with optional TLS
startWarpServer :: Warp.Settings -> FilePath -> TLSConfig -> Application -> IO ()
startWarpServer settings stateDir tlsConfig app =
  tlsConfigResolve stateDir tlsConfig >>= \case
    Nothing -> Warp.runSettings settings app
    Just tlsSettings -> WarpTLS.runTLS tlsSettings settings app
