{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Servant combinator for type-safe GitHub webhook handling
module Servant.GitHub.Webhook (
  -- * Servant combinator
  GitHubEvent (..),

  -- ** Security
  GitHubKey (..),
)
where

import Data.Aeson (FromJSON)
import Data.Aeson.Decoding (eitherDecodeStrict)
import Data.List (lookup)
import GitHub.Data.Webhooks.Events
import Network.Wai (requestHeaders, strictRequestBody)
import Servant
import Servant.Server.Internal

{- | Type class for GitHub webhook events.

Each event type must specify its event name as it appears in the
@X-GitHub-Event@ header.

Usage in Servant API:

@
type API = GitHubEvent PullRequestEvent :> Post '[JSON] NoContent
@
-}
class GitHubEvent (payload :: Type) where
  eventName :: ByteString

{- | Key provider for GitHub webhook signature verification.

Wraps an IO action that returns the webhook secret, allowing
for dynamic secret retrieval (e.g., from environment or vault).
-}
newtype GitHubKey = GitHubKey {unGitHubKey :: IO ByteString}

instance
  ( GitHubEvent payload
  , FromJSON payload
  , HasServer api ctx
  , HasContextEntry ctx GitHubKey
  ) =>
  HasServer (GitHubEvent payload :> api) ctx
  where
  type ServerT (GitHubEvent payload :> api) m = payload -> ServerT api m

  hoistServerWithContext _ ctx f server =
    hoistServerWithContext (Proxy @api) ctx f . server

  route _ ctx sub = route (Proxy @api) ctx $ addBodyCheck sub ctCheck bodyCheck
    where
      GitHubKey secret = getContextEntry ctx

      ctCheck = withRequest $ \req -> do
        let hdrs = requestHeaders req
        case lookup "X-GitHub-Event" hdrs of
          Just ev | ev == eventName @payload -> pass
          _ -> delayedFail err404
        msg <- liftIO $ toStrict <$> strictRequestBody req
        pure (msg, lookup "X-Hub-Signature-256" hdrs)

      bodyCheck (msg, sigHeader) = do
        key <- liftIO secret
        unless (verifySignature key msg sigHeader) $
          delayedFailFatal err401
        case eitherDecodeStrict msg of
          Left e -> delayedFailFatal err400 {errBody = encodeUtf8 e}
          Right v -> pure v

      -- \| Verify the GitHub webhook signature.
      --
      --      TODO: Implement actual HMAC-SHA256 verification using crypton
      --
      verifySignature :: ByteString -> ByteString -> Maybe ByteString -> Bool
      verifySignature _ _ _ = True

-- Instances for github-webhooks event types

instance GitHubEvent PullRequestEvent where
  eventName = "pull_request"

instance GitHubEvent PushEvent where
  eventName = "push"
