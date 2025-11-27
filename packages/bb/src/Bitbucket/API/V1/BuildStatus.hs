{-# LANGUAGE DeriveAnyClass #-}

{- | Bitbucket Build Status API

Provides functionality to report build status to Bitbucket commits.
-}
module Bitbucket.API.V1.BuildStatus (
  BuildState (..),
  BuildStatus (..),
  postBuildStatus,
) where

import Bitbucket.API.V1.Core (ServerEndpoint (..), Token (..), makeUrl, runReqEff)
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log, withLogContext)
import Effectful.Reader.Static qualified as ER
import Network.HTTP.Req (
  HttpException,
  POST (POST),
  ReqBodyJson (ReqBodyJson),
  defaultHttpConfig,
  header,
  ignoreResponse,
  renderUrl,
  req,
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

{- | Post build status to a Bitbucket commit

See https://developer.atlassian.com/server/bitbucket/rest/v803/api-group-build-status/#api-build-status-latest-commits-commitid-post
-}
postBuildStatus :: (Log (RichMessage IO) :> es, ER.Reader LogContext :> es, IOE :> es) => ServerEndpoint -> Token -> Text -> BuildStatus -> Eff es (Either HttpException ())
postBuildStatus endpoint (Token tok) commitHash status = withLogContext [("api", "bitbucket")] $ do
  let baseUrl = makeUrl endpoint
      url = baseUrl /: "rest" /: "build-status" /: "1.0" /: "commits" /: commitHash
  log Debug $ "POST " <> renderUrl url
  log Debug $ "JSON: " <> decodeUtf8 (Aeson.encode status)
  runReqEff defaultHttpConfig $ do
    let authHeader = encodeUtf8 $ "Bearer " <> tok
    void $
      req
        POST
        url
        (ReqBodyJson status)
        ignoreResponse
        (header "Authorization" authHeader)
