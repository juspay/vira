-- | This module can only be imported at top-level.
module Vira.App.LinkTo.Resolve where

import Servant.Links (Link, fieldLink)
import Vira.App ((//), (/:))
import Vira.App.LinkTo.Type
import Vira.Page.JobPage qualified as JobPage
import Vira.Page.RegistryPage qualified as RegistryPage
import Vira.Page.RepoPage qualified as RepoPage
import Vira.Routes
import Vira.Status qualified as Status

-- | Resolve a `LinkTo` into a servant `Link`
linkTo :: LinkTo -> Link
linkTo = \case
  Home -> fieldLink _home
  About -> fieldLink _about
  RepoListing -> fieldLink _repos // RegistryPage._listing
  Repo name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._view
  RepoUpdate name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._update
  Build repo branch -> fieldLink _jobs // JobPage._build /: repo /: branch
  Job jobId -> fieldLink _jobs // JobPage._view /: jobId
  JobLog jobId -> fieldLink _jobs // JobPage._rawLog /: jobId
  StatusGet -> fieldLink _status // Status._get
