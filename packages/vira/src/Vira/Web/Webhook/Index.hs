module Vira.Web.Webhook.Index where

import Servant
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings (..))
import Vira.Web.Webhook.GitHub qualified as WebhookGitHub

newtype Routes mode = Routes
  { _github :: mode :- "github" :> WebhookGitHub.Route
  }
  deriving stock (Generic)

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings =
  Routes
    { _github = WebhookGitHub.handler globalSettings viraRuntimeState webSettings
    }
