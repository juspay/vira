{-# LANGUAGE OverloadedRecordDot #-}

{- | Bitbucket Build Status API

Provides functionality to report build status to Bitbucket commits.
-}
module Bitbucket.API.V1.BuildStatus (
  BuildState (..),
  BuildStatus (..),
  postBuildStatus,
) where

import Bitbucket.API.V1.Core (BitbucketConfig (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Network.HTTP.Req (
  POST (POST),
  ReqBodyJson (ReqBodyJson),
  defaultHttpConfig,
  header,
  ignoreResponse,
  renderUrl,
  req,
  runReq,
  (/:),
 )

-- | Build state for Bitbucket commit status
data BuildState
  = Successful
  | Failed
  | InProgress
  deriving stock (Show, Eq)

instance ToJSON BuildState where
  toJSON = \case
    Successful -> "SUCCESSFUL"
    Failed -> "FAILED"
    InProgress -> "INPROGRESS"

-- | Build status information to report to Bitbucket
data BuildStatus = BuildStatus
  { state :: BuildState
  -- ^ Current build state
  , key :: Text
  -- ^ Unique identifier for this build
  , name :: Text
  -- ^ Display name for the build
  , url :: Text
  -- ^ URL to view build details
  , description :: Text
  -- ^ Description of the build status
  }
  deriving stock (Show, Eq)

instance ToJSON BuildStatus where
  toJSON status =
    object
      [ "state" .= status.state
      , "key" .= status.key
      , "name" .= status.name
      , "url" .= status.url
      , "description" .= status.description
      ]

{- | Post build status to a Bitbucket commit

Respects HTTPS_PROXY environment variable automatically via req library.
-}
postBuildStatus :: BitbucketConfig -> Text -> BuildStatus -> IO ()
postBuildStatus config commitHash status = do
  let url = config.baseUrl /: "rest" /: "build-status" /: "1.0" /: "commits" /: commitHash
  putTextLn $ "[bb] POST " <> renderUrl url
  putTextLn $ "[bb] JSON: " <> decodeUtf8 (Aeson.encode status)
  hFlush stdout
  runReq defaultHttpConfig $ do
    let authHeader = encodeUtf8 $ "Bearer " <> config.token

    void $
      req
        POST
        url
        (ReqBodyJson status)
        ignoreResponse
        (header "Authorization" authHeader)
