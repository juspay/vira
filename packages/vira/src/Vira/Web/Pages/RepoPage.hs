{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Web.Pages.RepoPage (
  Routes (..),
  handlers,
) where

import Data.Text qualified as T
import Data.Time (diffUTCTime)
import Effectful (Eff)
import Effectful.Colog.Simple (withLogContext)
import Effectful.Error.Static (throwError)
import Effectful.Git (BranchName (..), Commit (..), RepoName (..))
import Htmx.Lucid.Core (hxGet_, hxSwapS_, hxTarget_, hxTrigger_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (..))
import Lucid
import Lucid.Htmx.Contrib (hxConfirm_, hxPostSafe_)
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.Refresh qualified as Refresh
import Vira.Refresh.Type (RefreshOutcome (..), RefreshPriority (Now), RefreshResult (..), RefreshStatus (..))
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (BranchDetails (..), BranchQuery (..), PRCommit (..), PullRequest (..), jobEndTime)
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLink, getLinkUrl, runAppHtml)
import Vira.Web.Pages.PullPage qualified as PullPage
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Alert qualified as W
import Vira.Web.Widgets.Button qualified as W
import Vira.Web.Widgets.Commit qualified as W
import Vira.Web.Widgets.JobsListing qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Vira.Web.Widgets.Modal (ErrorModal (..))
import Vira.Web.Widgets.Status qualified as Status
import Vira.Web.Widgets.Time qualified as Time
import Web.TablerIcons.Outline qualified as Icon

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _update :: mode :- "fetch" Servant.:> Post '[HTML] (Headers '[HXRefresh] (Maybe ErrorModal))
  , _delete :: mode :- "delete" Servant.:> Post '[HTML] (Headers '[HXRedirect] Text)
  , _filterBranches :: mode :- "branches" Servant.:> QueryParam "q" Text :> Get '[HTML] (Html ())
  , _filterPRs :: mode :- "pulls" Servant.:> QueryParam "q" Text :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

crumbs :: [LinkTo.LinkTo]
crumbs = [LinkTo.RepoListing]

-- | Maximum number of items to display per section
maxDisplayed :: Int
maxDisplayed = 20

-- | Servant handlers for 'Routes'
handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> RepoName -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings name = do
  Routes
    { _view = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml $ viewHandler name
    , _update = Web.runAppInServant globalSettings viraRuntimeState webSettings $ updateHandler name
    , _delete = Web.runAppInServant globalSettings viraRuntimeState webSettings $ deleteHandler name
    , _filterBranches = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml . filterBranchesHandler name
    , _filterPRs = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml . filterPRsHandler name
    }

viewHandler :: RepoName -> AppHtml ()
viewHandler name = do
  repo <- lift $ App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  let query = BranchQuery {repoName = Just name, branchNamePattern = Nothing, neverBuilt = Nothing}
  branchDetails <- lift $ App.query (St.QueryBranchDetailsA query (fromIntegral maxDisplayed + 1))
  let isPruned = length branchDetails > maxDisplayed
      displayed = take maxDisplayed branchDetails
  W.layout (crumbs <> [LinkTo.Repo name]) $ viewRepo repo displayed isPruned

filterBranchesHandler :: RepoName -> Maybe Text -> AppHtml ()
filterBranchesHandler name mQuery = do
  let query = BranchQuery {repoName = Just name, branchNamePattern = mQuery, neverBuilt = Nothing}
  branchDetails <- lift $ App.query (St.QueryBranchDetailsA query (fromIntegral maxDisplayed + 1))
  let isPruned = length branchDetails > maxDisplayed
      displayed = take maxDisplayed branchDetails
  _ <- lift $ App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  viewBranchListing displayed isPruned

filterPRsHandler :: RepoName -> Maybe Text -> AppHtml ()
filterPRsHandler name mQuery = do
  _ <- lift $ App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  prs <- lift $ App.query (St.GetPullRequestsByRepoA name)
  let filtered = case mQuery of
        Nothing -> prs
        Just q -> filter (\pr -> T.isInfixOf (T.toLower q) (T.toLower pr.title)) prs
  prData <- forM filtered $ \pr -> do
    unapproved <- lift $ App.query (St.GetUnapprovedPRCommitsA name pr.prNumber)
    let branchRef = BranchName $ "refs/pull/" <> show pr.prNumber <> "/head"
    jobs <- lift $ App.query (St.GetJobsByBranchA name branchRef)
    pure (pr, unapproved, viaNonEmpty head jobs)
  viewPRListing prData

updateHandler :: RepoName -> Eff Web.AppServantStack (Headers '[HXRefresh] (Maybe ErrorModal))
updateHandler name = do
  withLogContext [("repo", show name)] $ do
    Refresh.scheduleRepoRefresh (one name) Now
    pure $ addHeader True Nothing

deleteHandler :: RepoName -> Eff Web.AppServantStack (Headers '[HXRedirect] Text)
deleteHandler name = do
  App.query (St.GetRepoByNameA name) >>= \case
    Just _repo -> do
      -- Delete from acid-state (repo, branches, jobs)
      -- Note: Runtime state (refresh state) is auto-cleaned by the refresh daemon
      App.update (St.DeleteRepoByNameA name) >>= \case
        Left errMsg -> throwError $ err400 {errBody = encodeUtf8 errMsg}
        Right () -> do
          redirectUrl <- getLinkUrl LinkTo.RepoListing
          pure $ addHeader redirectUrl "Ok"
    Nothing ->
      throwError err404

viewRepo :: St.Repo -> [BranchDetails] -> Bool -> AppHtml ()
viewRepo repo branchDetails isPruned = do
  -- Fetch current refresh status to show errors
  refreshStatus <- lift $ Refresh.getRepoRefreshStatus repo.name

  -- Repository header with smart refresh button
  W.viraPageHeaderWithIcon_
    (toHtmlRaw Icon.book_2)
    (toText $ toString repo.name)
    ( div_ [class_ "flex items-center justify-between"] $ do
        p_ [class_ "text-gray-600 dark:text-gray-300 text-sm font-mono break-all"] $
          toHtml repo.cloneUrl
        div_ [class_ "ml-4"] $
          Status.viraSmartRefreshButton_ repo.name
    )

  -- Show error alert if last refresh failed
  case refreshStatus of
    Completed RefreshResult {outcome = Failure errorMsg} ->
      div_ [class_ "mb-6"] $
        W.viraAlertWithTitle_ W.AlertError "Repository refresh failed" $
          toHtml errorMsg
    _ -> pass

  W.viraSection_ [] $ do
    -- Pull Requests section
    do
      prs <- lift $ App.query (St.GetPullRequestsByRepoA repo.name)
      prData <- forM prs $ \pr -> do
        unapproved <- lift $ App.query (St.GetUnapprovedPRCommitsA repo.name pr.prNumber)
        let branchRef = BranchName $ "refs/pull/" <> show pr.prNumber <> "/head"
        jobs <- lift $ App.query (St.GetJobsByBranchA repo.name branchRef)
        pure (pr, unapproved, viaNonEmpty head jobs)
      viewPullRequestsSection repo.name prData

    -- Branch listing
    div_ [class_ "bg-white dark:bg-gray-800 rounded-xl border border-gray-200 dark:border-gray-700 p-4 lg:p-8"] $ do
      -- Branch listing header
      div_ [class_ "mb-8"] $ do
        div_ [class_ "flex items-center mb-3"] $ do
          div_ [class_ "text-gray-600 dark:text-gray-300 w-8 h-8 mr-3 flex items-center justify-center"] $ toHtmlRaw Icon.git_branch
          h2_ [class_ "text-2xl font-bold text-gray-800 dark:text-gray-100"] "Branches"
          div_ [class_ "ml-auto text-sm text-gray-500 dark:text-gray-400"] $
            toHtml $
              show @Text (length branchDetails) <> " branches"
        div_ [class_ "h-px bg-gray-200 dark:bg-gray-700"] mempty

      -- Branch filter input
      div_ [class_ "mb-6"] $ do
        filterUrl <- lift $ getLinkUrl $ LinkTo.RepoBranchFilter repo.name
        div_ [class_ "relative"] $ do
          input_
            [ type_ "text"
            , class_ "w-full px-3 py-2 text-sm border border-gray-300 dark:border-gray-600 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 bg-white dark:bg-gray-700 dark:text-gray-100 transition-colors duration-200 pr-10"
            , placeholder_ "Filter branches..."
            , name_ "q"
            , hxGet_ filterUrl
            , hxTarget_ "#branch-listing"
            , hxTrigger_ "keyup changed delay:300ms"
            ]
          div_ [class_ "absolute inset-y-0 right-0 flex items-center pr-3 pointer-events-none"] $ do
            div_ [class_ "text-gray-500 dark:text-gray-400 w-4 h-4 flex items-center justify-center"] $ toHtmlRaw Icon.search

      -- Branch listing
      div_ [id_ "branch-listing"] $ do
        if null branchDetails
          then div_ [class_ "text-center py-12"] $ do
            div_ [class_ "text-gray-500 dark:text-gray-400 mb-4"] "No branches found"
            div_ [class_ "text-sm text-gray-400 dark:text-gray-500"] "Click Refresh to fetch branches from remote"
          else viewBranchListing branchDetails isPruned

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

-- | Branch listing fragment (used by both full page and HTMX filter)
viewBranchListing :: [BranchDetails] -> Bool -> AppHtml ()
viewBranchListing branchDetails isPruned = do
  when isPruned $
    div_ [class_ "mb-6 p-3 bg-yellow-50 dark:bg-yellow-900/20 border border-yellow-200 dark:border-yellow-800 rounded-lg"] $ do
      div_ [class_ "flex items-center text-sm text-yellow-800 dark:text-yellow-200"] $ do
        div_ [class_ "w-4 h-4 mr-2 flex items-center justify-center"] $ toHtmlRaw Icon.alert_circle
        span_ $ toHtml $ "Showing first " <> show @Text maxDisplayed <> " branches. Use the filter to narrow results."

  div_ [class_ "mt-4"] $ do
    forM_ branchDetails $ \details ->
      div_ [data_ "branch-item" (toText details.branch.branchName)] $
        W.viraBranchDetailsRow_ False details

-- | Pull requests section with filter input
viewPullRequestsSection :: RepoName -> [(PullRequest, [PRCommit], Maybe St.Job)] -> AppHtml ()
viewPullRequestsSection repoName prData =
  div_ [class_ "bg-white dark:bg-gray-800 rounded-xl border border-gray-200 dark:border-gray-700 p-4 lg:p-8"] $ do
    -- Header
    div_ [class_ "mb-8"] $ do
      div_ [class_ "flex items-center mb-3"] $ do
        div_ [class_ "text-gray-600 dark:text-gray-300 w-8 h-8 mr-3 flex items-center justify-center"] $ toHtmlRaw Icon.git_pull_request
        h2_ [class_ "text-2xl font-bold text-gray-800 dark:text-gray-100"] "Pull Requests"
        div_ [class_ "ml-auto text-sm text-gray-500 dark:text-gray-400"] $
          toHtml $
            show @Text (length prData) <> " pull requests"
      div_ [class_ "h-px bg-gray-200 dark:bg-gray-700"] mempty

    -- PR filter input
    div_ [class_ "mb-6"] $ do
      filterUrl <- lift $ getLinkUrl $ LinkTo.RepoPRFilter repoName
      div_ [class_ "relative"] $ do
        input_
          [ type_ "text"
          , class_ "w-full px-3 py-2 text-sm border border-gray-300 dark:border-gray-600 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 bg-white dark:bg-gray-700 dark:text-gray-100 transition-colors duration-200 pr-10"
          , placeholder_ "Filter pull requests..."
          , name_ "q"
          , hxGet_ filterUrl
          , hxTarget_ "#pr-listing"
          , hxTrigger_ "keyup changed delay:300ms"
          ]
        div_ [class_ "absolute inset-y-0 right-0 flex items-center pr-3 pointer-events-none"] $ do
          div_ [class_ "text-gray-500 dark:text-gray-400 w-4 h-4 flex items-center justify-center"] $ toHtmlRaw Icon.search

    -- PR listing
    div_ [id_ "pr-listing"] $
      viewPRListing prData

-- | PR listing fragment (used by both full page and HTMX filter)
viewPRListing :: [(PullRequest, [PRCommit], Maybe St.Job)] -> AppHtml ()
viewPRListing prData =
  if null prData
    then div_ [class_ "text-center py-12"] $ do
      div_ [class_ "text-gray-500 dark:text-gray-400 mb-4"] "No pull requests found"
      div_ [class_ "text-sm text-gray-400 dark:text-gray-500"] "Pull requests will appear here when opened via GitHub webhooks"
    else div_ [class_ "mt-4"] $
      forM_ prData $ \(pr, unapproved, mJob) ->
        viewPRRow pr unapproved mJob

{- | A single PR row — same card format as 'W.viraBranchDetailsRow_'.

If unapproved commits exist, shows the latest one with an Approve button
(like the Build button for NeverBuilt branches). Otherwise shows the
latest job with status badge.
-}
viewPRRow :: PullRequest -> [PRCommit] -> Maybe St.Job -> AppHtml ()
viewPRRow pr unapproved mJob = do
  prDetailUrl <- lift $ getLinkUrl $ LinkTo.RepoPull pr.repoName pr.prNumber

  div_ [class_ "relative mb-6"] $ do
    -- PR tag
    div_ [class_ "absolute -top-3 left-3 flex items-center z-10"] $
      a_ [href_ prDetailUrl, class_ "flex items-center gap-1 px-3 py-1 bg-blue-100 dark:bg-blue-900 border border-blue-300 dark:border-blue-700 rounded-full shadow-sm hover:opacity-70 transition-opacity"] $ do
        div_ [class_ "w-4 h-4 flex items-center justify-center text-blue-700 dark:text-blue-200 shrink-0"] $ toHtmlRaw Icon.git_pull_request
        span_ [class_ "text-sm font-semibold text-blue-900 dark:text-blue-100"] $ toHtml $ "PR " <> show @Text pr.prNumber

    -- Card
    a_
      [ href_ prDetailUrl
      , class_ "block pt-6 pb-4 px-4 rounded-lg bg-gray-50 dark:bg-gray-800 hover:bg-gray-100 dark:hover:bg-gray-700 border-2 border-gray-200 dark:border-gray-700 transition-all cursor-pointer"
      ]
      $ do
        div_ [class_ "grid grid-cols-1 lg:grid-cols-12 gap-3 items-center"] $ do
          case unapproved of
            (pc : _) -> do
              -- Unapproved commit: show commit info + Approve button
              maybeCommit <- lift $ App.query $ St.GetCommitByIdA pc.sha
              div_ [class_ "lg:col-span-8 flex items-center gap-2 flex-wrap text-sm"] $ do
                W.viraCommitHash_ pc.sha
                whenJust maybeCommit $ \commit ->
                  unless (T.null commit.message) $ do
                    span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
                    span_
                      [ class_ "text-gray-700 dark:text-gray-300 truncate max-w-md"
                      , title_ commit.message
                      ]
                      $ toHtml commit.message
              div_ [class_ "lg:col-span-4 flex items-center justify-start lg:justify-end gap-2 flex-wrap"] $ do
                approveLink <- lift $ getLink $ LinkTo.RepoPullApprove pr.repoName pr.prNumber pc.sha
                W.viraButton_
                  W.ButtonSuccess
                  [ hxPostSafe_ approveLink
                  , hxSwapS_ AfterEnd
                  , onclick_ "event.preventDefault(); event.stopPropagation();"
                  , class_ "!px-3 !py-1.5 !text-xs"
                  ]
                  $ do
                    W.viraButtonIcon_ $ toHtmlRaw Icon.shield_check
                    "Approve"
            [] -> case mJob of
              Just job -> do
                -- Built: show job commit info + status
                maybeCommit <- lift $ App.query $ St.GetCommitByIdA job.commit
                div_ [class_ "lg:col-span-8 flex items-center gap-2 flex-wrap text-sm"] $ do
                  W.viraCommitHash_ job.commit
                  whenJust maybeCommit $ \commit ->
                    unless (T.null commit.message) $ do
                      span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
                      span_
                        [ class_ "text-gray-700 dark:text-gray-300 truncate max-w-md"
                        , title_ commit.message
                        ]
                        $ toHtml commit.message
                  span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
                  div_ [class_ "text-xs text-gray-500 dark:text-gray-400"] $
                    Time.viraRelativeTime_ job.jobCreatedTime
                div_ [class_ "lg:col-span-4 flex items-center justify-start lg:justify-end gap-2 flex-wrap"] $ do
                  span_ [class_ "text-sm text-gray-600 dark:text-gray-400"] $ "#" <> toHtml (show @Text job.jobId)
                  span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
                  case jobEndTime job of
                    Just endTime -> Time.viraDuration_ $ diffUTCTime endTime job.jobCreatedTime
                    Nothing -> mempty
                  Status.viraStatusBadge_ job.jobStatus
              Nothing -> do
                -- No job, no unapproved: show PR title + state
                div_ [class_ "lg:col-span-8 flex items-center gap-2 flex-wrap text-sm"] $ do
                  PullPage.prStateBadge_ pr.prState
                  span_ [class_ "text-gray-700 dark:text-gray-300 truncate max-w-md"] $ toHtml pr.title
                  PullPage.forkBadge_ pr.forkRepo
                div_ [class_ "lg:col-span-4"] mempty
