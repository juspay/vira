-- | GitHub App webhook handler for receiving events
module Vira.Web.Webhook.GitHub (
  Route,
  handler,
) where

import Colog (Severity (..))
import Effectful.Colog.Simple (log)
import GitHub.Data.Webhooks.Events (PullRequestEvent)
import Servant
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (..))
import Vira.App qualified as App
import Vira.App.CLI (WebSettings (..))
import Vira.Web.Stack qualified as Web

-- TODO: Bind Github Event type to the Github ReqBody type in the type-system without having to rely on value-level dynamic dispatch of type-level (by defining a sum-type of all the supported events)
type Route =
  GitHubEvent '[ 'WebhookPullRequestEvent]
    :> GitHubSignedReqBody '[JSON] PullRequestEvent
    :> Post '[JSON] NoContent

-- | Webhook route handlers
handler :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> Server Route
handler globalSettings viraRuntimeState webSettings _eventInfo _payload =
  Web.runAppInServant globalSettings viraRuntimeState webSettings $ do
    log Info "Received pull_request event"

    pure NoContent

-- handleWebhook :: RepoWebhookEvent -> PullRequestEvent -> Eff AppServantStack NoContent
-- handleWebhook _eventInfo _event = do
--   log Info "Received pull_request event"

--   pure NoContent

-- handlePR :: Value -> Eff AppServantStack ()
-- handlePR _payload = do
--   log Info "Received pull_request event"
