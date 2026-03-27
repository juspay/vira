{-# LANGUAGE DeriveAnyClass #-}

{- | RSA Key Reading

Utilities for reading RSA private keys from PEM files.

Based on https://github.com/serokell/github-app/blob/1ac32618ed039dccb0ec9c44a8160e582928bd44/src/Crypto/PubKey/RSA/Read.hs
-}
module Vira.Lib.Crypto (
  ReadRsaKeyError (..),
  readRsaPem,
) where

import Control.Exception (throwIO)
import Crypto.PubKey.RSA (PrivateKey)
import Data.X509 (PrivKey (PrivKeyRSA))
import Data.X509.Memory (readKeyFileFromMemory)

data ReadRsaKeyError
  = SingleKeyExpected Int
  | NotRsaKey
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

-- | Read an RSA private key from PEM-encoded bytes. Throws on error.
readRsaPem :: (MonadIO m) => ByteString -> m PrivateKey
readRsaPem bs =
  case readKeyFileFromMemory bs of
    [PrivKeyRSA rsaKey] -> pure rsaKey
    [_] -> liftIO $ throwIO NotRsaKey
    keys -> liftIO $ throwIO $ SingleKeyExpected (length keys)
