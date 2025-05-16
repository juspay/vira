-- | This module can only be imported at top-level.
module Vira.App.LinkTo.Resolve where

import Servant.Links (Link, fieldLink)
import Vira.App ((//), (/:))
import Vira.App.LinkTo.Type
import Vira.Page.BranchJobsPage qualified as BranchJobsPage
import Vira.Page.JobLog qualified as JobLog
import Vira.Page.JobPage qualified as JobPage
import Vira.Page.RegistryPage qualified as RegistryPage
import Vira.Page.RepoPage qualified as RepoPage
import Vira.Routes

-- | Resolve a `LinkTo` into a servant `Link`
linkTo :: LinkTo -> Link
linkTo = \case
  Home -> fieldLink _home
  About -> fieldLink _about
  RepoListing -> fieldLink _repos // RegistryPage._listing
  Repo name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._view
  RepoUpdate name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._update
  Build repo branch -> fieldLink _jobs // JobPage._build /: repo /: branch
  BranchJobs repo branch -> fieldLink _repos // RegistryPage._repo /: repo // RepoPage._branchJobs /: branch // BranchJobsPage._view
  Job jobId -> fieldLink _jobs // JobPage._view /: jobId
  JobLog jobId -> fieldLink _jobs // JobPage._log /: jobId // JobLog._rawLog
  JobLogStream jobId -> fieldLink _jobs // JobPage._log /: jobId // JobLog._streamLog
  StatusGet -> fieldLink _status
