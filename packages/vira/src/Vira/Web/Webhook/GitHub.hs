-- | GitHub App webhook handler for receiving events
module Vira.Web.Webhook.GitHub where

import Colog (Severity (..))
import Effectful (Eff)
import Effectful.Colog.Simple (log)
import GitHub.Data.Webhooks.Events (PullRequestEvent, PushEvent)
import Servant
import Servant.GitHub.Webhook (GitHubEvent)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.Web.Stack (AppServantStack)
import Vira.Web.Stack qualified as Web

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
