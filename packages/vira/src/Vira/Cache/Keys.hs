{-# LANGUAGE TemplateHaskell #-}

{- | Cache key generation and management

Provides automatic generation and management of Nix binary cache signing keys,
following the pattern of TLS certificate auto-generation.
-}
module Vira.Cache.Keys (
  -- * Key generation
  ensureCacheKeys,

  -- * Key reading
  readSecretKey,
  readPublicKey,
) where

import Data.Char (isSpace)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Process (callProcess)
import System.Which (staticWhich)

{- | Path to the `nix-store` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
nixStore :: FilePath
nixStore = $(staticWhich "nix-store")

{- | Ensure cache signing keys exist, generating if necessary

Returns a tuple of (secretKeyPath, publicKeyPath).

The keys are stored in @<cacheDir>/@:
- @secret-key@ - Private signing key
- @public-key@ - Public key for cache users

If keys already exist, they are reused. Otherwise, new keys are generated
using @nix-store --generate-binary-cache-key@.
-}
ensureCacheKeys :: FilePath -> IO (FilePath, FilePath)
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

  pure (secretKeyPath, publicKeyPath)

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
readSecretKey :: FilePath -> IO ByteString
readSecretKey path = do
  content <- readFileBS path
  let contentText :: Text = decodeUtf8 content
  let stripped :: String = filter (not . isSpace) (toString contentText)
  pure $ encodeUtf8 (toText stripped)

{- | Read the public key

Reads the public key for display in the UI.
Users need this to configure their nix.conf.
-}
readPublicKey :: FilePath -> IO Text
readPublicKey path = do
  content <- readFileBS path
  let contentText :: Text = decodeUtf8 content
  let stripped :: String = filter (not . isSpace) (toString contentText)
  pure $ toText stripped
