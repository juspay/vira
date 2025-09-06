{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.RepoPage (
  Routes (..),
  handlers,
) where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Git qualified as Git
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (AfterEnd))
import Lucid
import Lucid.Htmx.Contrib (hxConfirm_, hxPostSafe_)
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type
import Vira.Widgets.Button qualified as W
import Vira.Widgets.Code qualified as W
import Vira.Widgets.Form qualified as W
import Vira.Widgets.Layout qualified as W
import Vira.Widgets.Status qualified as Status
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _update :: mode :- "fetch" :> Post '[HTML] (Headers '[HXRefresh] Text)
  , _delete :: mode :- "delete" :> Post '[HTML] (Headers '[HXRedirect] Text)
  }
  deriving stock (Generic)

crumbs :: [LinkTo.LinkTo]
crumbs = [LinkTo.RepoListing]

handlers :: App.AppState -> WebSettings -> RepoName -> Routes AsServer
handlers cfg webSettings name = do
  Routes
    { _view = App.runAppInServant cfg webSettings . App.runAppHtml $ viewHandler name
    , _update = App.runAppInServant cfg webSettings $ updateHandler name
    , _delete = App.runAppInServant cfg webSettings $ deleteHandler name
    }

viewHandler :: RepoName -> AppHtml ()
viewHandler name = do
  repo <- lift $ App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  branches <- lift $ App.query $ St.GetBranchesByRepoA name
  allJobs <- lift $ App.query $ St.GetJobsByRepoA repo.name
  W.layout (crumbs <> [LinkTo.Repo name]) $ viewRepo repo branches allJobs

updateHandler :: RepoName -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
updateHandler name = do
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  allBranches <- Git.remoteBranches repo.cloneUrl
  App.update $ St.SetRepoBranchesA repo.name allBranches
  pure $ addHeader True "Ok"

deleteHandler :: RepoName -> Eff App.AppServantStack (Headers '[HXRedirect] Text)
deleteHandler name = do
  App.query (St.GetRepoByNameA name) >>= \case
    Just _repo -> do
      App.update $ St.DeleteRepoByNameA name
      redirectUrl <- App.getLinkUrl LinkTo.RepoListing
      pure $ addHeader redirectUrl "Ok"
    Nothing ->
      throwError err404

viewRepo :: St.Repo -> [St.Branch] -> [St.Job] -> App.AppHtml ()
viewRepo repo branches _allJobs = do
  -- Repository header with only refresh button
  updateLink <- lift $ App.getLink $ LinkTo.RepoUpdate repo.name
  W.viraPageHeaderWithIcon_
    (toHtmlRaw Icon.book_2)
    (toText $ toString repo.name)
    ( div_ [class_ "flex items-center justify-between"] $ do
        p_ [class_ "text-gray-600 text-sm font-mono break-all"] $
          toHtml repo.cloneUrl
        div_ [class_ "flex items-center gap-2 ml-4"] $ do
          W.viraButton_
            W.ButtonSecondary
            [ hxPostSafe_ updateLink
            , hxSwapS_ AfterEnd
            , title_ "Refresh branches"
            ]
            $ do
              W.viraButtonIcon_ $ toHtmlRaw Icon.refresh
              "Refresh"
    )

  W.viraSection_ [] $ do
    -- Branch listing
    div_ [class_ "bg-white rounded-xl border border-gray-200 p-4 lg:p-8"] $ do
      -- Branch listing header
      div_ [class_ "mb-8"] $ do
        div_ [class_ "flex items-center mb-3"] $ do
          div_ [class_ "text-gray-600 w-8 h-8 mr-3 flex items-center justify-center"] $ toHtmlRaw Icon.git_branch
          h2_ [class_ "text-2xl font-bold text-gray-800"] "Branches"
          div_ [class_ "ml-auto text-sm text-gray-500"] $
            toHtml $
              show @Text (length branches) <> " branches"
        div_ [class_ "h-px bg-gray-200"] mempty

      -- Branch filter input
      div_ [class_ "mb-6"] $ do
        W.viraFilterInput_
          "[data-branch-item]"
          [placeholder_ "Filter branches...", id_ "branch-filter"]

        -- Filter results counter (updated by JavaScript)
        div_ [class_ "mt-2 text-xs text-gray-600", id_ "branch-count"] mempty

      -- Branch listing
      if null branches
        then div_ [class_ "text-center py-12"] $ do
          div_ [class_ "text-gray-500 mb-4"] "No branches found"
          div_ [class_ "text-sm text-gray-400"] "Click Refresh to fetch branches from remote"
        else viewBranchListing repo branches

    -- Delete button at bottom
    div_ [class_ "mt-8 pt-8 border-t border-gray-200"] $ do
      deleteLink <- lift $ App.getLink $ LinkTo.RepoDelete repo.name
      div_ [class_ "bg-red-50 border border-red-200 rounded-lg p-6"] $ do
        div_ [class_ "flex items-start"] $ do
          div_ [class_ "flex-shrink-0"] $ do
            div_ [class_ "w-8 h-8 bg-red-100 rounded-full flex items-center justify-center"] $ do
              div_ [class_ "w-4 h-4 text-red-600"] $ toHtmlRaw Icon.alert_triangle
          div_ [class_ "ml-3 flex-1"] $ do
            h3_ [class_ "text-sm font-medium text-red-800"] "Delete Repository"
            p_
              [class_ "mt-1 text-sm text-red-700"]
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
viewBranchListing :: St.Repo -> [St.Branch] -> App.AppHtml ()
viewBranchListing repo branches = do
  -- Get latest job for each branch for status indicators and sorting
  branchStatuses <- lift $ forM branches $ \branch -> do
    jobs <- App.query $ St.GetJobsByBranchA repo.name branch.branchName
    let maybeLatestJob = viaNonEmpty head jobs
        effectiveStatus = getBranchEffectiveStatus branch maybeLatestJob
    pure (branch, maybeLatestJob, effectiveStatus)

  -- Sort branches: built/building first, never built last
  let sortedBranchStatuses = sortOn (\(_, maybeJob, _) -> Down $ isJust maybeJob) branchStatuses

  div_ [class_ "space-y-4"] $ do
    forM_ sortedBranchStatuses $ \(branch, maybeLatestJob, effectiveStatus) -> do
      branchUrl <- lift $ App.getLinkUrl $ LinkTo.RepoBranch repo.name branch.branchName
      let branchNameText = toText $ toString branch.branchName
      a_ [href_ branchUrl, class_ "block p-6 rounded-lg hover:bg-gray-50 transition-colors border border-gray-200 hover:border-gray-300", data_ "branch-item" branchNameText] $ do
        div_ [class_ "flex items-start justify-between"] $ do
          div_ [class_ "flex-1"] $ do
            -- Branch name and icon
            div_ [class_ "flex items-center mb-3"] $ do
              div_ [class_ "w-5 h-5 mr-3 flex items-center justify-center text-gray-600"] $ toHtmlRaw Icon.git_branch
              h3_ [class_ "text-xl font-semibold text-gray-900"] $
                toHtml $
                  toString branch.branchName

            -- Latest commit info
            div_ [class_ "flex items-center space-x-3 mb-2"] $ do
              span_ [class_ "text-sm text-gray-600"] "Latest commit:"
              div_ [class_ "flex items-center space-x-2"] $ do
                div_ [class_ "w-4 h-4 flex items-center justify-center text-gray-500"] $ toHtmlRaw Icon.git_commit
                W.viraCommitInfo_ branch.headCommit

            -- Build info (job ID and count if available)
            case maybeLatestJob of
              Just latestJob -> do
                jobs <- lift $ App.query $ St.GetJobsByBranchA repo.name branch.branchName
                div_ [class_ "flex items-center space-x-4 text-sm text-gray-600"] $ do
                  span_ $ "Latest build: #" <> toHtml (show @Text latestJob.jobId)
                  span_ $ toHtml (show @Text (length jobs)) <> " total builds"
              Nothing ->
                div_ [class_ "text-sm text-gray-500"] "No builds yet"

          -- Status badge (larger and more prominent)
          div_ [class_ "flex-shrink-0 ml-6"] $ do
            case effectiveStatus of
              NeverBuilt ->
                span_ [class_ "inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-gray-100 text-gray-800"] "Never built"
              JobStatus jobStatus ->
                Status.viraStatusBadge_ jobStatus
              OutOfDate ->
                span_ [class_ "inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-orange-100 text-orange-800"] $ do
                  div_ [class_ "w-4 h-4 mr-2 flex items-center justify-center"] $ toHtmlRaw Icon.clock
                  "Out of date"

-- Data type to represent the effective status of a branch
data BranchEffectiveStatus
  = NeverBuilt
  | JobStatus St.JobStatus
  | OutOfDate

-- Determine the effective status of a branch considering if it's out of date
getBranchEffectiveStatus :: St.Branch -> Maybe St.Job -> BranchEffectiveStatus
getBranchEffectiveStatus branch = \case
  Nothing -> NeverBuilt
  Just job ->
    if branch.headCommit.commitId == job.jobCommit
      then JobStatus job.jobStatus
      else OutOfDate
