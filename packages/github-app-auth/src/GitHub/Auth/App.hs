{- | GitHub App Authentication

Like [github-app](https://github.com/serokell/github-app) but simpler: no token caching, just stateless JWT creation and token exchange.

This module handles JWT creation and token exchange, allowing you to make
authenticated GitHub API requests on behalf of a GitHub App installation.

JWT creation requires a private key.
For managing private keys for your GitHub App, see <https://docs.github.com/en/apps/creating-github-apps/authenticating-with-a-github-app/managing-private-keys-for-github-apps>

Example usage:

@
result <- withAppAuth (fetchAccessToken appId installationId privateKey) $ \auth -> do
  GitHub.github auth someRequest
@
-}
module GitHub.Auth.App (
  withAppAuth,
  fetchAccessToken,
  AppAuthError (..),
) where

import Control.Lens ((?~))
import Crypto.JOSE qualified as JOSE
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
import Crypto.PubKey.RSA (PrivateKey)
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GitHub.Auth (Auth (..))
import GitHub.Data.Apps (AccessToken (..), App, Installation)
import GitHub.Data.Id (Id, untagId)
import GitHub.Endpoints.Apps (createAccessTokenR)
import GitHub.Request (github)

-- | Errors that can occur during GitHub App authentication
data AppAuthError
  = JWTCreationFailed JOSE.Error
  | -- | Failed to exchange JWT for access token
    TokenExchangeFailed Text
  deriving stock (Show, Eq)

{- | Run an action with GitHub App authentication.

Takes an action that fetches an access token (e.g., 'fetchAccessToken')
and runs the provided action with the fetched token.
-}
withAppAuth ::
  -- | Token provider (e.g., cached, from env, or via 'fetchAccessToken')
  IO (Either AppAuthError AccessToken) ->
  (Auth -> IO a) ->
  IO (Either AppAuthError a)
withAppAuth fetchToken action = do
  token <- fetchToken
  case token of
    Left e -> pure $ Left e
    Right t -> Right <$> action (OAuth $ encodeUtf8 t.accessTokenToken)

fetchAccessToken :: Id App -> PrivateKey -> Id Installation -> IO (Either AppAuthError AccessToken)
fetchAccessToken appId key instId = runExceptT $ do
  jwt <- ExceptT $ createJWT appId key
  ExceptT $ exchangeJWT jwt instId

-- | Exchange a JWT for an installation access token.
exchangeJWT :: SignedJWT -> Id Installation -> IO (Either AppAuthError AccessToken)
exchangeJWT jwt installationId = do
  tokenResult <- github (JWT ((decodeUtf8 . encodeCompact) jwt)) (createAccessTokenR installationId)
  case tokenResult of
    Left e -> pure $ Left $ TokenExchangeFailed (show e)
    Right accessToken -> pure $ Right accessToken

{- | Create a JWT for GitHub App authentication

https://docs.github.com/en/apps/creating-github-apps/authenticating-with-a-github-app/generating-a-json-web-token-jwt-for-a-github-app#about-json-web-tokens-jwts
-}
createJWT :: Id App -> PrivateKey -> IO (Either AppAuthError SignedJWT)
createJWT appId privateKey = do
  now <- getCurrentTime
  let now' = truncateToSeconds now
      expiry = truncateToSeconds $ addUTCTime 600 now
      claims =
        emptyClaimsSet
          & claimIat
          ?~ NumericDate now'
            & claimExp
          ?~ NumericDate expiry
            & claimIss
          ?~ fromString (show $ untagId appId)
  first JWTCreationFailed <$> runJOSE (signClaims jwk header claims)
  where
    jwk = JOSE.fromRSA privateKey
    header = newJWSHeader ((), RS256)

    -- Truncate to whole seconds; GitHub requires integer Unix timestamps
    truncateToSeconds t =
      posixSecondsToUTCTime $ fromIntegral (floor (utcTimeToPOSIXSeconds t) :: Integer)
