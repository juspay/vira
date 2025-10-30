{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Web.Pages.RepoPage (
  Routes (..),
  handlers,
) where

import Effectful (Eff)
import Effectful.Colog.Simple (withLogContext)
import Effectful.Error.Static (throwError)
import Effectful.Git (RepoName)
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
import Vira.Refresh.Core qualified as Refresh
import Vira.Refresh.Type (RefreshPriority (Now))
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (BranchDetails (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLink, getLinkUrl, runAppHtml)
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Button qualified as W
import Vira.Web.Widgets.JobsListing qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Vira.Web.Widgets.Modal (ErrorModal (..))
import Vira.Web.Widgets.Status qualified as Status
import Web.TablerIcons.Outline qualified as Icon

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _update :: mode :- "fetch" Servant.:> Post '[HTML] (Headers '[HXRefresh] (Maybe ErrorModal))
  , _delete :: mode :- "delete" Servant.:> Post '[HTML] (Headers '[HXRedirect] Text)
  , _filterBranches :: mode :- "branches" Servant.:> QueryParam "q" Text :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

crumbs :: [LinkTo.LinkTo]
crumbs = [LinkTo.RepoListing]

-- | Maximum number of branches to display
maxBranchesDisplayed :: Int
maxBranchesDisplayed = 20

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> RepoName -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings name = do
  Routes
    { _view = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml $ viewHandler name
    , _update = Web.runAppInServant globalSettings viraRuntimeState webSettings $ updateHandler name
    , _delete = Web.runAppInServant globalSettings viraRuntimeState webSettings $ deleteHandler name
    , _filterBranches = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml . filterBranchesHandler name
    }

viewHandler :: RepoName -> AppHtml ()
viewHandler name = do
  repo <- lift $ App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  branchDetails <- lift $ App.query (St.GetAllBranchesA (Just name) Nothing (fromIntegral maxBranchesDisplayed + 1))
  let isPruned = length branchDetails > maxBranchesDisplayed
      displayed = take maxBranchesDisplayed branchDetails
  W.layout (crumbs <> [LinkTo.Repo name]) $ viewRepo repo displayed isPruned

filterBranchesHandler :: RepoName -> Maybe Text -> AppHtml ()
filterBranchesHandler name mQuery = do
  branchDetails <- lift $ App.query (St.GetAllBranchesA (Just name) mQuery (fromIntegral maxBranchesDisplayed + 1))
  let isPruned = length branchDetails > maxBranchesDisplayed
      displayed = take maxBranchesDisplayed branchDetails
  _ <- lift $ App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  viewBranchListing displayed isPruned

updateHandler :: RepoName -> Eff Web.AppServantStack (Headers '[HXRefresh] (Maybe ErrorModal))
updateHandler name = do
  withLogContext [("repo", show name)] $ do
    Refresh.scheduleRepoRefresh name Now
    pure $ addHeader True Nothing

deleteHandler :: RepoName -> Eff Web.AppServantStack (Headers '[HXRedirect] Text)
deleteHandler name = do
  App.query (St.GetRepoByNameA name) >>= \case
    Just _repo -> do
      App.update $ St.DeleteRepoByNameA name
      redirectUrl <- getLinkUrl LinkTo.RepoListing
      pure $ addHeader redirectUrl "Ok"
    Nothing ->
      throwError err404

viewRepo :: St.Repo -> [BranchDetails] -> Bool -> AppHtml ()
viewRepo repo branchDetails isPruned = do
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

-- Branch listing component for repository page
viewBranchListing :: [BranchDetails] -> Bool -> AppHtml ()
viewBranchListing branchDetails isPruned = do
  -- Pruning indicator
  when isPruned $
    div_ [class_ "mb-6 p-3 bg-yellow-50 dark:bg-yellow-900/20 border border-yellow-200 dark:border-yellow-800 rounded-lg"] $ do
      div_ [class_ "flex items-center text-sm text-yellow-800 dark:text-yellow-200"] $ do
        div_ [class_ "w-4 h-4 mr-2 flex items-center justify-center"] $ toHtmlRaw Icon.alert_circle
        span_ $ toHtml $ "Showing first " <> show @Text maxBranchesDisplayed <> " branches. Use the filter to narrow results."

  div_ [class_ "mt-4"] $ do
    forM_ branchDetails $ \details ->
      div_ [data_ "branch-item" (toText details.branch.branchName)] $
        W.viraBranchDetailsRow_ False details
