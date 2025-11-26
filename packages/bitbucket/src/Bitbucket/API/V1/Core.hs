{- | Core Bitbucket API functionality

Provides basic types and utilities for Bitbucket API operations.
-}
module Bitbucket.API.V1.Core (
  ServerEndpoint (..),
  Token (..),
  testConnection,
  makeUrl,
) where

import Control.Exception (catch)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Network.HTTP.Req (
  GET (GET),
  HttpException,
  NoReqBody (NoReqBody),
  Scheme (Https),
  Url,
  defaultHttpConfig,
  header,
  https,
  ignoreResponse,
  renderUrl,
  req,
  runReq,
  (/:),
 )

-- | Bitbucket server endpoint (host only, no protocol)
newtype ServerEndpoint = ServerEndpoint {host :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable)

-- | Bearer token for authentication
newtype Token = Token {unToken :: Text}
  deriving stock (Show, Eq)
  deriving newtype (FromJSON, ToJSON)

-- | Construct HTTPS URL from ServerEndpoint
makeUrl :: ServerEndpoint -> Url 'Https
makeUrl (ServerEndpoint h) = https h

{- | Test connection to Bitbucket API

Makes a simple API request to verify:
1. URL is reachable
2. API responds
3. Token is accepted
-}
testConnection :: ServerEndpoint -> Token -> IO (Either Text ())
testConnection endpoint (Token tok) = do
  let baseUrl = makeUrl endpoint
      url = baseUrl /: "rest" /: "api" /: "1.0" /: "projects"
  putTextLn $ "[bb] GET " <> renderUrl url
  hFlush stdout
  catch
    ( runReq defaultHttpConfig $ do
        let authHeader = encodeUtf8 $ "Bearer " <> tok
        void $
          req
            GET
            url
            NoReqBody
            ignoreResponse
            (header "Authorization" authHeader)
        pure (Right ())
    )
    (\(e :: HttpException) -> pure $ Left $ "HTTP error: " <> show e)
