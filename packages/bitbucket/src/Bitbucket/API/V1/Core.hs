{- | Core Bitbucket API functionality

Provides basic types and utilities for Bitbucket API operations.
-}
module Bitbucket.API.V1.Core (
  ServerEndpoint (..),
  Token (..),
  makeUrl,
) where

import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Network.HTTP.Req (
  Scheme (Https),
  Url,
  https,
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
