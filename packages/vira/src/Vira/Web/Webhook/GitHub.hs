{-# LANGUAGE AllowAmbiguousTypes #-}

-- | GitHub App webhook handler for receiving events
module Vira.Web.Webhook.GitHub where

import Colog (Severity (..))
import Data.Aeson (FromJSON)
import Data.Aeson.Decoding (eitherDecodeStrict)
import Data.List (lookup)
import Effectful (Eff)
import Effectful.Colog.Simple (log)
import GitHub.Data.Webhooks.Events (PullRequestEvent, PushEvent)
import Network.Wai (requestHeaders, strictRequestBody)
import Servant
import Servant.Server.Internal
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.Web.Stack (AppServantStack)
import Vira.Web.Stack qualified as Web

-- import Crypto.Hash.SHA256 (hmac)

{- | Matches the "X-GitHub-Event" header AND validates the Signature AND parses the body.
usage: GitHubEvent "pull_request" PullRequestEvent :> Post '[JSON] ()
data GitHubEvent (event :: Symbol) (payload :: Type)
-}
class GitHubEvent (payload :: Type) where
  eventName :: ByteString

instance GitHubEvent PullRequestEvent where
  eventName = "pull_request"

instance GitHubEvent PushEvent where
  eventName = "push"

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

verifySignature :: ByteString -> ByteString -> Maybe ByteString -> Bool
verifySignature _ _ _ = True

data Routes mode = Routes
  { _pr :: mode :- GitHubEvent PullRequestEvent :> Post '[JSON] NoContent
  , _push :: mode :- GitHubEvent PushEvent :> Post '[JSON] NoContent
  }
  deriving stock (Generic)

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings = do
  Routes
    { _pr = Web.runAppInServant globalSettings viraRuntimeState webSettings . prHandler
    , _push = Web.runAppInServant globalSettings viraRuntimeState webSettings . pushHandler
    }

prHandler :: PullRequestEvent -> Eff AppServantStack NoContent
prHandler _ = do
  log Info "Received Pull Request"
  pure NoContent

pushHandler :: PushEvent -> Eff AppServantStack NoContent
pushHandler _ = do
  log Info "Received Push"
  pure NoContent
