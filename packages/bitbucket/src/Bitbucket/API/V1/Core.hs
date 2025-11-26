{- | Core Bitbucket API functionality

Provides basic types and utilities for Bitbucket API operations.
-}
module Bitbucket.API.V1.Core (
  BitbucketConfig (..),
  testConnection,
) where

import Control.Exception (catch)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Network.HTTP.Req (
  GET (GET),
  HttpException,
  NoReqBody (NoReqBody),
  Scheme (Https),
  Url,
  defaultHttpConfig,
  header,
  ignoreResponse,
  renderUrl,
  req,
  runReq,
  useHttpsURI,
  (/:),
 )
import Text.URI (mkURI)

-- | Configuration for Bitbucket API access
data BitbucketConfig = BitbucketConfig
  { baseUrl :: Url 'Https
  -- ^ Bitbucket server base URL
  , token :: Text
  -- ^ Bearer token for authentication
  }
  deriving stock (Show)

instance ToJSON BitbucketConfig where
  toJSON (BitbucketConfig url tok) =
    object ["baseUrl" .= renderUrl url, "token" .= tok]

instance FromJSON BitbucketConfig where
  parseJSON = withObject "BitbucketConfig" $ \o -> do
    baseUrlText <- o .: "baseUrl"
    tokenText <- o .: "token"

    -- Parse URL
    uri <- case mkURI baseUrlText of
      Left err -> fail $ "Invalid URL: " <> show err
      Right u -> pure u

    url <- case useHttpsURI uri of
      Just (httpsUrl, _) -> pure httpsUrl
      Nothing -> fail $ "URL must use HTTPS scheme: " <> toString baseUrlText

    pure $ BitbucketConfig url tokenText

{- | Test connection to Bitbucket API

Makes a simple API request to verify:
1. URL is reachable
2. API responds
3. Token is accepted
-}
testConnection :: BitbucketConfig -> IO (Either Text ())
testConnection (BitbucketConfig baseUrl token) = do
  let url = baseUrl /: "rest" /: "api" /: "1.0" /: "projects"
  putTextLn $ "[bb] GET " <> renderUrl url
  hFlush stdout
  catch
    ( runReq defaultHttpConfig $ do
        let authHeader = encodeUtf8 $ "Bearer " <> token
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
