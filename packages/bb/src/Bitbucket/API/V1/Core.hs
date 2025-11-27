{- | Core Bitbucket API functionality

Provides basic types and utilities for Bitbucket API operations.

Note: We only support Bitbucket Server, not Bitbucket Cloud. Hence V1.
See https://community.atlassian.com/forums/Bitbucket-questions/Does-the-rest-api-2-0-exist-for-server/qaq-p/881631
-}
module Bitbucket.API.V1.Core (
  ServerEndpoint (..),
  Token (..),
  makeUrl,
  runReqEff,
) where

import Control.Exception (try)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Effectful (Eff, IOE, (:>))
import Network.HTTP.Req (
  HttpConfig,
  HttpException,
  Req,
  Scheme (Https),
  Url,
  https,
  runReq,
 )

-- | Bitbucket server endpoint (host only, no protocol)
newtype ServerEndpoint = ServerEndpoint {host :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable, IsString)

-- | Bearer token for authentication
newtype Token = Token {unToken :: Text}
  deriving stock (Show, Eq)
  deriving newtype (FromJSON, ToJSON)

-- | Construct HTTPS URL from ServerEndpoint
makeUrl :: ServerEndpoint -> Url 'Https
makeUrl (ServerEndpoint h) = https h

{- | Run a req operation with HttpException catching in Eff

Catches HttpException and returns it as Either.
-}
runReqEff :: (IOE :> es) => HttpConfig -> Req a -> Eff es (Either HttpException a)
runReqEff config action = liftIO $ try $ runReq config action
