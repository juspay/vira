{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub Checks API - Authentication helpers

This module provides GitHub App authentication functionality for the Checks API.
The types and request builders are re-exported from the @github@ package.
-}
module Vira.Webhook.GitHub.ChecksAPI (
  ChecksAPIError (..),
  withAppAuth,
) where

import Control.Lens ((?~))
import Crypto.JOSE (Error, JWK, fromRSA)
import Crypto.JOSE.Compact (encodeCompact)
import Crypto.JOSE.JWA.JWS (Alg (RS256))
import Crypto.JOSE.JWS (newJWSHeader)
import Crypto.JWT (
  NumericDate (..),
  SignedJWT,
  claimExp,
  claimIat,
  claimIss,
  emptyClaimsSet,
  runJOSE,
  signClaims,
 )
import Data.ByteString.Lazy qualified as LBS
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.X509 (PrivKey (..))
import Data.X509.Memory (readKeyFileFromMemory)
import Effectful (Eff, IOE, (:>))
import GitHub qualified as GH
import GitHub.Auth (Auth (..))
import GitHub.Data (App, Id, Installation)
import GitHub.Endpoints.Apps (createAccessTokenR)
import GitHub.Request (github)
import Network.HTTP.Req (
  HttpException,
 )

type PrivateKeyPath = FilePath

withAppAuth :: (IOE :> es) => Id App -> Id Installation -> PrivateKeyPath -> (Auth -> Eff es a) -> Eff es (Either ChecksAPIError a)
withAppAuth appId installationId privateKey action = do
  jwt <- liftIO $ createJWT appId privateKey
  case jwt of
    Left err -> pure $ Left err
    Right jwt' -> do
      accessToken <- liftIO $ github (JWT (decodeUtf8 @Text jwt')) (createAccessTokenR installationId)
      case accessToken of
        Left err -> pure $ Left $ TokenExchangeError (show err)
        Right accessToken' -> Right <$> action (OAuth $ encodeUtf8 accessToken'.accessTokenToken)

-- | Errors that can occur when using the Checks API
data ChecksAPIError
  = JWTCreationError Text
  | TokenExchangeError Text
  | CheckRunCreationError Text
  | PrivateKeyError Text
  | NoInstallationId
  | HttpError HttpException
  deriving stock (Show)

-- | Read RSA private key from PEM file and convert to JWK
readPrivateKey :: FilePath -> IO (Either ChecksAPIError JWK)
readPrivateKey path = do
  content <- readFileBS path
  case readKeyFileFromMemory content of
    [PrivKeyRSA rsaKey] -> pure $ Right $ fromRSA rsaKey
    [] -> pure $ Left $ PrivateKeyError "No RSA key found in PEM file"
    _ -> pure $ Left $ PrivateKeyError "Multiple keys or non-RSA key in PEM file"

-- | Create a JWT for GitHub App authentication
createJWT :: Id App -> PrivateKeyPath -> IO (Either ChecksAPIError LBS.ByteString)
createJWT appId privateKeyPath = do
  now <- getCurrentTime
  keyResult <- readPrivateKey privateKeyPath
  case keyResult of
    Left err -> pure $ Left err
    Right jwk -> do
      -- Truncate to whole seconds to avoid scientific notation in JSON
      -- GitHub requires integer Unix timestamps, not floats
      let nowSeconds = truncateToSeconds now
          expSeconds = truncateToSeconds $ addUTCTime 600 now
          claims =
            emptyClaimsSet
              & claimIat
              ?~ NumericDate nowSeconds
                & claimExp
              ?~ NumericDate expSeconds
                & claimIss
              ?~ fromString (show appId)
      result :: Either Error SignedJWT <- runJOSE $ signClaims jwk (newJWSHeader ((), RS256)) claims
      case result of
        Left err -> pure $ Left $ JWTCreationError $ show err
        Right signed -> pure $ Right $ encodeCompact signed
  where
    -- Truncate UTCTime to whole seconds (removes fractional seconds)
    truncateToSeconds t =
      posixSecondsToUTCTime $ fromIntegral (floor (utcTimeToPOSIXSeconds t) :: Integer)
