{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

-- Based on https://github.com/serokell/github-app/blob/1ac32618ed039dccb0ec9c44a8160e582928bd44/src/Crypto/PubKey/RSA/Read.hs

module Crypto.PubKey.RSA.Read (
  ReadRsaKeyError (..),
  readRsaPem,
) where

import Crypto.PubKey.RSA (PrivateKey)
import Data.X509 (PrivKey (PrivKeyRSA))
import Data.X509.Memory (readKeyFileFromMemory)

data ReadRsaKeyError
  = SingleKeyExpected {rrkeActualKeysCount :: Int}
  | NotRsaKey
  deriving stock (Show)

readRsaPem :: ByteString -> Either ReadRsaKeyError PrivateKey
readRsaPem bs =
  case readKeyFileFromMemory bs of
    [k] -> case k of
      PrivKeyRSA rsaKey -> Right rsaKey
      _ -> Left NotRsaKey
    l -> Left $ SingleKeyExpected (length l)
