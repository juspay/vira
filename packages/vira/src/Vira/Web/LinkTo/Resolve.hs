-- | This module can only be imported at top-level.
module Vira.Web.LinkTo.Resolve where

import Servant.Links (Link, fieldLink)
import Vira.Web.LinkTo.Type
import Vira.Web.Pages.BranchPage qualified as BranchPage
import Vira.Web.Pages.CachePage qualified as CachePage
import Vira.Web.Pages.EnvironmentPage qualified as EnvironmentPage
import Vira.Web.Pages.EventsPage qualified as EventsPage
import Vira.Web.Pages.IndexPage
import Vira.Web.Pages.JobLog qualified as JobLog
import Vira.Web.Pages.JobPage qualified as JobPage
import Vira.Web.Pages.RegistryPage qualified as RegistryPage
import Vira.Web.Pages.RepoPage qualified as RepoPage
import Vira.Web.Servant ((//), (/:))

-- | Resolve a 'LinkTo' into a servant 'Link'
linkTo :: LinkTo -> Link
linkTo = \case
  Home neverBuilt -> fieldLink _home (Just neverBuilt)
  RepoListing -> fieldLink _repos // RegistryPage._listing
  Repo name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._view
  RepoUpdate name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._update
  RepoDelete name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._delete
  RepoAdd -> fieldLink _repos // RegistryPage._addRepo
  RepoBranchFilter name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._filterBranches /: (Nothing :: Maybe Text)
  Build repo branch -> fieldLink _jobs // JobPage._build /: repo /: branch
  RepoBranch repo branch -> fieldLink _repos // RegistryPage._branch /: repo /: branch // BranchPage._view
  Job jobId -> fieldLink _jobs // JobPage._view /: jobId
  JobLog jobId -> fieldLink _jobs // JobPage._log /: jobId // JobLog._rawLog
  JobLogStream jobId -> fieldLink _jobs // JobPage._log /: jobId // JobLog._streamLog
  Kill jobId -> fieldLink _jobs // JobPage._kill /: jobId
  Environment -> fieldLink _environment // EnvironmentPage._view
  Cache -> fieldLink _cache // CachePage._view
  Events -> fieldLink _events // EventsPage._view
  Refresh mPatterns -> fieldLink _refresh mPatterns
