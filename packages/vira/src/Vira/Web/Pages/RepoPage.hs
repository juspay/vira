{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Web.Pages.RepoPage (
  Routes (..),
  handlers,
) where

import Data.Time (UTCTime (..), diffUTCTime)
import Data.Time.Calendar (fromGregorian)
import Effectful (Eff, IOE, raise, (:>))
import Effectful.Error.Static (runErrorNoCallStack, throwError)
import Effectful.Git (RepoName)
import Effectful.Git qualified as Git
import Effectful.Git.Mirror qualified as Mirror
import Effectful.Reader.Dynamic (Reader, asks)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (..))
import Lucid
import Lucid.Htmx.Contrib (hxConfirm_, hxPostSafe_)
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.CI.Workspace qualified as Workspace
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLink, getLinkUrl, runAppHtml)
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Button qualified as W
import Vira.Web.Widgets.Commit qualified as W
import Vira.Web.Widgets.Form qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Vira.Web.Widgets.Modal (ErrorModal (..))
import Vira.Web.Widgets.Status qualified as Status
import Vira.Web.Widgets.Time qualified as Time
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (Reader, ask, asks)

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _update :: mode :- "fetch" Servant.:> Post '[HTML] (Headers '[HXRefresh] (Maybe ErrorModal))
  , _delete :: mode :- "delete" Servant.:> Post '[HTML] (Headers '[HXRedirect] Text)
  }
  deriving stock (Generic)

crumbs :: [LinkTo.LinkTo]
crumbs = [LinkTo.RepoListing]

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> RepoName -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings name = do
  Routes
    { _view = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml $ viewHandler name
    , _update = Web.runAppInServant globalSettings viraRuntimeState webSettings $ updateHandler name
    , _delete = Web.runAppInServant globalSettings viraRuntimeState webSettings $ deleteHandler name
    }

viewHandler :: RepoName -> AppHtml ()
viewHandler name = do
  repo <- lift $ App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  branches <- lift $ App.query $ St.GetBranchesByRepoA name
  allJobs <- lift $ App.query $ St.GetJobsByRepoA repo.name
  W.layout (crumbs <> [LinkTo.Repo name]) $ viewRepo repo branches allJobs

updateHandler :: RepoName -> Eff Web.AppServantStack (Headers '[HXRefresh] (Maybe ErrorModal))
updateHandler name = do
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  supervisor <- asks @App.ViraRuntimeState (.supervisor)
  let mirrorPath = Workspace.mirrorPath supervisor repo.name
  result <- runErrorNoCallStack @Text $ do
    -- Ensure mirror exists and update it
    Mirror.syncMirror repo.cloneUrl mirrorPath
    allBranches <- Git.remoteBranchesFromClone mirrorPath
    raise $ App.update $ St.SetRepoBranchesA repo.name allBranches

  case result of
    Left errorMsg ->
      pure $ noHeader $ Just (ErrorModal errorMsg)
    Right () -> pure $ addHeader True Nothing

deleteHandler :: RepoName -> Eff Web.AppServantStack (Headers '[HXRedirect] Text)
deleteHandler name = do
  App.query (St.GetRepoByNameA name) >>= \case
    Just _repo -> do
      App.update $ St.DeleteRepoByNameA name
      redirectUrl <- getLinkUrl LinkTo.RepoListing
      pure $ addHeader redirectUrl "Ok"
    Nothing ->
      throwError err404

viewRepo :: St.Repo -> [St.Branch] -> [St.Job] -> AppHtml ()
viewRepo repo branches _allJobs = do
  -- Repository header with only refresh button
  updateLink <- lift $ getLink $ LinkTo.RepoUpdate repo.name
  W.viraPageHeaderWithIcon_
    (toHtmlRaw Icon.book_2)
    (toText $ toString repo.name)
    ( div_ [class_ "flex items-center justify-between"] $ do
        p_ [class_ "text-gray-600 dark:text-gray-300 text-sm font-mono break-all"] $
          toHtml repo.cloneUrl
        div_ [class_ "flex items-center gap-2 ml-4"] $ do
          W.viraRequestButton_
            W.ButtonSecondary
            updateLink
            [title_ "Refresh branches"]
            $ do
              W.viraButtonIcon_ $ toHtmlRaw Icon.refresh
              "Refresh"
    )

  W.viraSection_ [] $ do
    -- Branch listing
    div_ [class_ "bg-white dark:bg-gray-800 rounded-xl border border-gray-200 dark:border-gray-700 p-4 lg:p-8"] $ do
      -- Branch listing header
      div_ [class_ "mb-8"] $ do
        div_ [class_ "flex items-center mb-3"] $ do
          div_ [class_ "text-gray-600 dark:text-gray-300 w-8 h-8 mr-3 flex items-center justify-center"] $ toHtmlRaw Icon.git_branch
          h2_ [class_ "text-2xl font-bold text-gray-800 dark:text-gray-100"] "Branches"
          div_ [class_ "ml-auto text-sm text-gray-500 dark:text-gray-400"] $
            toHtml $
              show @Text (length branches) <> " branches"
        div_ [class_ "h-px bg-gray-200 dark:bg-gray-700"] mempty

      -- Branch filter input
      div_ [class_ "mb-6"] $ do
        W.viraFilterInput_
          "[data-branch-item]"
          [placeholder_ "Filter branches...", id_ "branch-filter"]

        -- Filter results counter (updated by JavaScript)
        div_ [class_ "mt-2 text-xs text-gray-600 dark:text-gray-400", id_ "branch-count"] mempty

      -- Branch listing
      if null branches
        then div_ [class_ "text-center py-12"] $ do
          div_ [class_ "text-gray-500 dark:text-gray-400 mb-4"] "No branches found"
          div_ [class_ "text-sm text-gray-400 dark:text-gray-500"] "Click Refresh to fetch branches from remote"
        else viewBranchListing repo branches

    -- Delete button at bottom
    div_ [class_ "mt-8 pt-8 border-t border-gray-200 dark:border-gray-700"] $ do
      deleteLink <- lift $ getLink $ LinkTo.RepoDelete repo.name
      div_ [class_ "bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg p-6"] $ do
        div_ [class_ "flex items-start"] $ do
          div_ [class_ "flex-shrink-0"] $ do
            div_ [class_ "w-8 h-8 bg-red-100 dark:bg-red-900/30 rounded-full flex items-center justify-center"] $ do
              div_ [class_ "w-4 h-4 text-red-600 dark:text-red-400"] $ toHtmlRaw Icon.alert_triangle
          div_ [class_ "ml-3 flex-1"] $ do
            h3_ [class_ "text-sm font-medium text-red-800 dark:text-red-200"] "Delete Repository"
            p_
              [class_ "mt-1 text-sm text-red-700 dark:text-red-300"]
              "Permanently delete this repository and all its associated data. This action cannot be undone."
          div_ [class_ "ml-4 flex-shrink-0"] $ do
            W.viraButton_
              W.ButtonDestructive
              [ hxPostSafe_ deleteLink
              , hxSwapS_ AfterEnd
              , hxConfirm_ "Are you sure you want to delete this repository? This action cannot be undone."
              , title_ "Delete repository"
              ]
              $ do
                W.viraButtonIcon_ $ toHtmlRaw Icon.trash
                "Delete Repository"

-- Branch listing component for repository page
viewBranchListing :: St.Repo -> [St.Branch] -> AppHtml ()
viewBranchListing repo branches = do
  branchStatuses <- lift $ mkBranchStatus repo.name `mapM` branches

  div_ [class_ "space-y-2"] $ do
    forM_ (sort branchStatuses) $ \branchStatus -> do
      branchUrl <- lift $ getLinkUrl $ LinkTo.RepoBranch repo.name branchStatus.branchData.branchName
      a_
        [ href_ branchUrl
        , class_ "block p-3 rounded-lg hover:bg-gray-50 dark:hover:bg-gray-700 transition-colors border border-gray-200 dark:border-gray-700 hover:border-gray-300 dark:hover:border-gray-600"
        , data_ "branch-item" (toText branchStatus.branchData.branchName)
        ]
        $ do
          -- Single-line columnar layout for easy scanning
          div_ [class_ "grid grid-cols-12 gap-4 items-center"] $ do
            -- Column 1: Branch name (4 columns)
            div_ [class_ "col-span-4 flex items-center space-x-2 min-w-0"] $ do
              div_ [class_ "w-4 h-4 flex items-center justify-center text-gray-600 dark:text-gray-400"] $ toHtmlRaw Icon.git_branch
              h3_ [class_ "text-sm font-semibold text-gray-900 dark:text-gray-100 truncate"] $
                toHtml $
                  toText branchStatus.branchData.branchName

            -- Column 2: Last update info (5 columns)
            div_ [class_ "col-span-5 min-w-0"] $ do
              W.viraCommitInfoCompact_ branchStatus.mHeadCommit

            -- Column 3: Build info and status (3 columns)
            div_ [class_ "col-span-3 flex items-center justify-end space-x-2"] $ do
              -- Build duration and metadata
              case branchStatus.mLatestJob of
                Just latestJob -> do
                  jobs <- lift $ App.query $ St.GetJobsByBranchA repo.name branchStatus.branchData.branchName
                  div_ [class_ "flex items-center space-x-2 text-xs text-gray-500 dark:text-gray-400"] $ do
                    case St.jobEndTime latestJob of
                      Just endTime -> do
                        let duration = diffUTCTime endTime latestJob.jobCreatedTime
                        Time.viraDuration_ duration
                      Nothing -> mempty
                    span_ $ "#" <> toHtml (show @Text latestJob.jobId)
                    span_ $ "(" <> toHtml (show @Text (length jobs)) <> ")"
                Nothing ->
                  span_ [class_ "text-xs text-gray-500 dark:text-gray-400"] "No builds"

              -- Status badge
              viewBranchEffectiveStatus branchStatus.effectiveStatus

-- | Data type to hold branch information for sorting and display
data BranchStatus = BranchStatus
  { branchData :: St.Branch
  -- ^ The branch information from the database
  , mLatestJob :: Maybe St.Job
  -- ^ The most recent CI job for this branch, if any
  , effectiveStatus :: BranchEffectiveStatus
  -- ^ The computed status of the branch (built, building, never built, or out of date)
  , mHeadCommit :: Maybe Git.Commit
  -- ^ The commit at the head of the branch, if available
  }
  deriving stock (Show, Eq)

{- | Prioritizes branches with CI jobs (built/building) over never-built branches.
Within each group, sorts by commit date descending (most recent first) to surface
recently active branches. This helps users quickly identify branches that need
attention or are actively being worked on.
-}
instance Ord BranchStatus where
  compare a b = compare (sortingKey a) (sortingKey b)
    where
      sortingKey bs = (Down $ isJust bs.mLatestJob, Down $ maybe defaultTime (.date) bs.mHeadCommit)
      defaultTime = UTCTime (fromGregorian 1900 1 1) 0

-- | Create a 'BranchStatus' for a given branch, fetching required data.
mkBranchStatus :: (Reader App.ViraRuntimeState Effectful.:> es, IOE Effectful.:> es) => RepoName -> St.Branch -> Eff es BranchStatus
mkBranchStatus repoName branch = do
  jobs <- App.query $ St.GetJobsByBranchA repoName branch.branchName
  let mLatestJob = viaNonEmpty head jobs
      effectiveStatus = getBranchEffectiveStatus branch mLatestJob
  mHeadCommit <- App.query $ St.GetCommitByIdA branch.headCommit
  pure BranchStatus {branchData = branch, mLatestJob, effectiveStatus, mHeadCommit}

{- | The effective build-status of a branch.

This type represents the computed status of a branch based on its CI job history
and current head commit. Used in 'BranchStatus' and computed by
'getBranchEffectiveStatus'.
-}
data BranchEffectiveStatus
  = -- | The branch has never been built by CI
    NeverBuilt
  | -- | The branch has been built, with the given job status
    JobStatus St.JobStatus
  | -- | The branch was built previously but the head commit has changed
    OutOfDate
  deriving stock (Show, Eq, Ord)

-- | Determine the 'BranchEffectiveStatus' based on its latest job.
getBranchEffectiveStatus :: St.Branch -> Maybe St.Job -> BranchEffectiveStatus
getBranchEffectiveStatus branch = \case
  Nothing -> NeverBuilt
  Just job ->
    if branch.headCommit == job.commit
      then JobStatus job.jobStatus
      else OutOfDate

-- | Render the status badge for a branch's effective status.
viewBranchEffectiveStatus :: BranchEffectiveStatus -> AppHtml ()
viewBranchEffectiveStatus = \case
  NeverBuilt ->
    span_ [class_ "inline-flex items-center px-2 py-1 rounded-full text-xs font-medium bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300"] "Never built"
  JobStatus jobStatus ->
    Status.viraStatusBadge_ jobStatus
  OutOfDate ->
    span_ [class_ "inline-flex items-center px-2 py-1 rounded-full text-xs font-medium bg-orange-100 dark:bg-orange-900/30 text-orange-700 dark:text-orange-300"] $ do
      div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.clock
      "Out of date"
