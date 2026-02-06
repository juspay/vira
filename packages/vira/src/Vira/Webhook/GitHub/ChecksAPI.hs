{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub Checks API - Authentication helpers

This module provides GitHub App authentication functionality for the Checks API.
The types and request builders are re-exported from the @github@ package.
-}
module Vira.Webhook.GitHub.ChecksAPI (
  InstallationToken (..),
  ChecksAPIError (..),
  createJWT,
  getInstallationToken,
) where

import Control.Exception (try)
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
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.X509 (PrivKey (..))
import Data.X509.Memory (readKeyFileFromMemory)
import Effectful (Eff, IOE, (:>))
import Network.HTTP.Req (
  HttpException,
  POST (POST),
  ReqBodyJson (ReqBodyJson),
  defaultHttpConfig,
  header,
  https,
  jsonResponse,
  req,
  responseBody,
  runReq,
  (/:),
 )
import Vira.App.CLI (GitHubAppSettings (..))

-- | Installation access token from GitHub
data InstallationToken = InstallationToken
  { installationTokenToken :: Text
  , installationTokenExpiresAt :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON InstallationToken where
  parseJSON = Aeson.withObject "InstallationToken" $ \v ->
    InstallationToken
      <$> v .: "token"
      <*> v .: "expires_at"

-- | Errors that can occur when using the Checks API
data ChecksAPIError
  = JWTCreationError Text
  | TokenExchangeError Text
  | CheckRunCreationError Text
  | PrivateKeyError Text
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
createJWT :: GitHubAppSettings -> IO (Either ChecksAPIError LBS.ByteString)
createJWT settings = do
  now <- getCurrentTime
  keyResult <- readPrivateKey settings.privateKeyPath
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
              ?~ fromString (show settings.appId)
      result :: Either Error SignedJWT <- runJOSE $ signClaims jwk (newJWSHeader ((), RS256)) claims
      case result of
        Left err -> pure $ Left $ JWTCreationError $ show err
        Right signed -> pure $ Right $ encodeCompact signed
  where
    -- Truncate UTCTime to whole seconds (removes fractional seconds)
    truncateToSeconds t =
      posixSecondsToUTCTime $ fromIntegral (floor (utcTimeToPOSIXSeconds t) :: Integer)

-- | Exchange JWT for an installation access token
getInstallationToken ::
  (IOE :> es) =>
  LBS.ByteString ->
  Int ->
  Eff es (Either ChecksAPIError InstallationToken)
getInstallationToken jwt installationId = do
  result <- liftIO $ try @HttpException $ runReq defaultHttpConfig $ do
    let url = https "api.github.com" /: "app" /: "installations" /: show installationId /: "access_tokens"
        authHeader = "Bearer " <> toStrict jwt
    resp <-
      req
        POST
        url
        (ReqBodyJson (Aeson.object []))
        jsonResponse
        ( header "Authorization" authHeader
            <> header "Accept" "application/vnd.github+json"
            <> header "User-Agent" "Vira-CI"
        )
    pure $ responseBody resp
  pure $ first HttpError result
