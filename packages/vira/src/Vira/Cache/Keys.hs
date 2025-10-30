{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Cache key generation and management

Provides automatic generation and management of Nix binary cache signing keys,
following the pattern of TLS certificate auto-generation.
-}
module Vira.Cache.Keys (
  -- * Types
  CacheKeys (..),
  SecretKey,
  PublicKey,

  -- * Conversion
  secretKeyByteString,

  -- * Key generation
  ensureCacheKeys,
) where

import Data.Char (isSpace)
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
ensureCacheKeys :: FilePath -> IO CacheKeys
ensureCacheKeys cacheDir = do
  let (secretKeyPath, publicKeyPath) = keyPaths cacheDir

  secretExists <- doesFileExist secretKeyPath
  publicExists <- doesFileExist publicKeyPath

  if secretExists && publicExists
    then do
      putStrLn $ "Using existing cache keys from " <> cacheDir <> "/"
    else do
      putStrLn "Generating cache signing keys..."
      createDirectoryIfMissing True cacheDir
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
generateKeys :: FilePath -> IO ()
generateKeys cacheDir = do
  let (secretKeyPath, publicKeyPath) = keyPaths cacheDir

  -- Generate keys using nix-store --generate-binary-cache-key
  -- Key name is "cache" - this appears in the signature
  callProcess
    nixStore
    [ "--generate-binary-cache-key"
    , "cache"
    , secretKeyPath
    , publicKeyPath
    ]

  putStrLn "Generated cache signing keys:"
  putStrLn $ "  Secret key: " <> secretKeyPath
  putStrLn $ "  Public key: " <> publicKeyPath

{- | Read the secret signing key

Reads and strips whitespace from the secret key file.
Used for signing NARs in nix-serve-ng.
-}
readSecretKey :: FilePath -> IO SecretKey
readSecretKey path = do
  content <- readFileBS path
  let contentText :: Text = decodeUtf8 content
  let stripped :: String = filter (not . isSpace) (toString contentText)
  pure $ SecretKey $ encodeUtf8 (toText stripped)

{- | Read the public key

Reads the public key for display in the UI.
Users need this to configure their nix.conf.
-}
readPublicKey :: FilePath -> IO PublicKey
readPublicKey path = do
  content <- readFileBS path
  let contentText :: Text = decodeUtf8 content
  let stripped :: String = filter (not . isSpace) (toString contentText)
  pure $ PublicKey $ toText stripped
