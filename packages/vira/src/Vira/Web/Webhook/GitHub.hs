-- | GitHub App webhook handler for receiving events
module Vira.Web.Webhook.GitHub (
  Routes (..),
  handlers,
) where

import Colog (Severity (..))
import Data.Aeson (Value)
import Effectful (Eff)
import Effectful.Colog.Simple (log)
import Servant
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.Web.Stack (AppServantStack)
import Vira.Web.Stack qualified as Web

-- | GitHub webhook routes
newtype Routes mode = Routes
  { _receive ::
      mode
        :- Header "X-GitHub-Event" Text
          :> Header "X-Hub-Signature-256" Text
          :> Header "X-GitHub-Delivery" Text
          :> ReqBody '[JSON] Value
          :> Post '[JSON] NoContent
  }
  deriving stock (Generic)

-- | Webhook route handlers
handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings =
  Routes
    { _receive = \mEvent mSig mDelivery payload ->
        Web.runAppInServant globalSettings viraRuntimeState webSettings $
          handleWebhook mEvent mSig mDelivery payload
    }

-- | Handle incoming GitHub webhook
handleWebhook ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Value ->
  Eff AppServantStack NoContent
handleWebhook mEventType _mSignature mDeliveryId payload = do
  log Info $ "GitHub webhook received: event=" <> fromMaybe "unknown" mEventType <> ", delivery=" <> fromMaybe "unknown" mDeliveryId
  case mEventType of
    Just "pull_request" -> handlePullRequest payload
    Just other -> log Debug $ "Ignoring event type: " <> other
    Nothing -> log Warning "Missing X-GitHub-Event header"
  pure NoContent

-- | Handle pull_request events
handlePullRequest :: Value -> Eff AppServantStack ()
handlePullRequest _payload = do
  -- TODO: Parse payload to extract PR details (action, number, repo, etc.)
  log Info "Received pull_request event"
