{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Cache key generation and management

Provides automatic generation and management of Nix binary cache signing keys,
following the pattern of TLS certificate auto-generation.
-}
module System.Nix.Cache.Keys (
  -- * Types
  CacheKeys (..),
  SecretKey,
  PublicKey,

  -- * Conversion
  secretKeyByteString,

  -- * Key generation
  ensureCacheKeys,
) where

import Colog.Message (RichMessage)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple
import Effectful.Reader.Static qualified as ER
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Process (callProcess)
import System.Which (staticWhich)

-- | Secret signing key for nix-serve-ng
newtype SecretKey = SecretKey ByteString

-- | Public key for nix.conf
newtype PublicKey = PublicKey Text
  deriving stock (Show)
  deriving newtype (IsString, ToString, ToText)

-- | Cache signing keys
data CacheKeys = CacheKeys
  { secretKey :: SecretKey
  -- ^ Private signing key for nix-serve-ng
  , publicKey :: PublicKey
  -- ^ Public key for users to add to nix.conf
  }

-- | Convert SecretKey to ByteString
secretKeyByteString :: SecretKey -> ByteString
secretKeyByteString (SecretKey bs) = bs

{- | Path to the `nix-store` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
nixStore :: FilePath
nixStore = $(staticWhich "nix-store")

{- | Ensure cache signing keys exist, generating if necessary

The keys are stored in @<cacheDir>/@:
- @secret-key@ - Private signing key
- @public-key@ - Public key for cache users

If keys already exist, they are reused. Otherwise, new keys are generated
using @nix-store --generate-binary-cache-key@.

Returns the keys as strings.
-}
ensureCacheKeys :: (IOE :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es) => FilePath -> Eff es CacheKeys
ensureCacheKeys cacheDir = do
  let (secretKeyPath, publicKeyPath) = keyPaths cacheDir

  secretExists <- liftIO $ doesFileExist secretKeyPath
  publicExists <- liftIO $ doesFileExist publicKeyPath

  if secretExists && publicExists
    then do
      log Info $ "Using existing cache keys from " <> toText cacheDir <> "/"
    else do
      log Info "Generating cache signing keys..."
      liftIO $ createDirectoryIfMissing True cacheDir
      generateKeys cacheDir

  -- Read and return keys
  secretKey <- readSecretKey secretKeyPath
  publicKey <- readPublicKey publicKeyPath
  pure CacheKeys {..}

-- | Helper to construct key file paths
keyPaths :: FilePath -> (FilePath, FilePath)
keyPaths cacheDir =
  let secretKeyPath = cacheDir </> "secret-key"
      publicKeyPath = cacheDir </> "public-key"
   in (secretKeyPath, publicKeyPath)

-- | Generate cache signing keys using nix-store
generateKeys :: (IOE :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es) => FilePath -> Eff es ()
generateKeys cacheDir = do
  let (secretKeyPath, publicKeyPath) = keyPaths cacheDir

  -- Generate keys using nix-store --generate-binary-cache-key
  -- Key name is "cache" - this appears in the signature
  liftIO $
    callProcess
      nixStore
      [ "--generate-binary-cache-key"
      , "cache"
      , secretKeyPath
      , publicKeyPath
      ]

  log Info "Generated cache signing keys"
  log Debug $ "  Secret key: " <> toText secretKeyPath
  log Debug $ "  Public key: " <> toText publicKeyPath

-- | Read and strip a key file
readKeyFile :: (IOE :> es) => FilePath -> Eff es Text
readKeyFile path = T.strip . decodeUtf8 <$> liftIO (readFileBS path)

{- | Read the secret signing key

Reads and strips whitespace from the secret key file.
Used for signing NARs in nix-serve-ng.
-}
readSecretKey :: (IOE :> es) => FilePath -> Eff es SecretKey
readSecretKey path = SecretKey . encodeUtf8 <$> readKeyFile path

{- | Read the public key

Reads the public key for display in the UI.
Users need this to configure their nix.conf.
-}
readPublicKey :: (IOE :> es) => FilePath -> Eff es PublicKey
readPublicKey path = PublicKey <$> readKeyFile path
