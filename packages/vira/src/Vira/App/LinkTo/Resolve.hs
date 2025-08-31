-- | This module can only be imported at top-level.
module Vira.App.LinkTo.Resolve where

import Servant.Links (Link, fieldLink)
import Vira.App ((//), (/:))
import Vira.App.LinkTo.Type
import Vira.Page.IndexPage
import Vira.Page.JobLog qualified as JobLog
import Vira.Page.JobPage qualified as JobPage
import Vira.Page.RegistryPage qualified as RegistryPage
import Vira.Page.RepoPage qualified as RepoPage
import Vira.Page.SettingsPage qualified as SettingsPage

-- | Resolve a `LinkTo` into a servant `Link`
linkTo :: LinkTo -> Link
linkTo = \case
  Home -> fieldLink _home
  RepoListing -> fieldLink _repos // RegistryPage._listing
  Repo name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._view
  RepoUpdate name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._update
  RepoDelete name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._delete
  RepoAdd -> fieldLink _repos // RegistryPage._addRepo
  Build repo branch -> fieldLink _jobs // JobPage._build /: repo /: branch
  RepoBranch repo branch -> fieldLink _repos // RegistryPage._repo /: repo // RepoPage._branch /: branch
  Job jobId -> fieldLink _jobs // JobPage._view /: jobId
  JobLog jobId -> fieldLink _jobs // JobPage._log /: jobId // JobLog._rawLog
  JobLogStream jobId -> fieldLink _jobs // JobPage._log /: jobId // JobLog._streamLog
  Kill jobId -> fieldLink _jobs // JobPage._kill /: jobId
  Settings -> fieldLink _settings // SettingsPage._view
  SettingsUpdateCachix -> fieldLink _settings // SettingsPage._updateCachix
  SettingsDeleteCachix -> fieldLink _settings // SettingsPage._deleteCachix
  SettingsUpdateAttic -> fieldLink _settings // SettingsPage._updateAttic
  SettingsDeleteAttic -> fieldLink _settings // SettingsPage._deleteAttic
  Refresh -> fieldLink _refresh
