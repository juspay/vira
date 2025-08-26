{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.RepoPage where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Dynamic (ask)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (AfterEnd))
import Lucid
import Lucid.Htmx.Contrib (hxConfirm_, hxPostSafe_)
import Servant hiding (throwError)
import Servant.API ((:>))
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Git (BranchName)
import Vira.Lib.Git qualified as Git
import Vira.Page.JobPage qualified as JobPage
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type
import Vira.Widgets.Button qualified as W
import Vira.Widgets.Card qualified as W
import Vira.Widgets.Form qualified as W
import Vira.Widgets.Layout qualified as W
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _update :: mode :- "fetch" :> Post '[HTML] (Headers '[HXRefresh] Text)
  , _delete :: mode :- "delete" :> Post '[HTML] (Headers '[HXRedirect] Text)
  , _branch :: mode :- "branches" Servant.API.:> Capture "name" BranchName :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

crumbs :: [LinkTo.LinkTo]
crumbs = [LinkTo.RepoListing]

handlers :: App.AppState -> RepoName -> Routes AsServer
handlers cfg name = do
  Routes
    { _view = App.runAppInServant cfg $ viewHandler name
    , _update = App.runAppInServant cfg $ updateHandler name
    , _delete = App.runAppInServant cfg $ deleteHandler name
    , _branch = App.runAppInServant cfg . branchViewHandler name
    }

branchViewHandler :: RepoName -> BranchName -> Eff App.AppServantStack (Html ())
branchViewHandler repoName branchName = do
  repo <- App.query (St.GetRepoByNameA repoName) >>= maybe (throwError err404) pure
  branch <- App.query (St.GetBranchByNameA repoName branchName) >>= maybe (throwError err404) pure
  branches <- App.query $ St.GetBranchesByRepoA repoName
  jobs <- App.query $ St.GetJobsByBranchA repoName branchName
  cfg <- ask
  let branchCrumbs = crumbs <> [LinkTo.Repo repoName, LinkTo.RepoBranch repoName branchName]
  pure $ W.layout cfg branchCrumbs $ do
    viewRepoBranch cfg.linkTo repo branch branches jobs

viewHandler :: RepoName -> Eff App.AppServantStack (Html ())
viewHandler name = do
  cfg <- ask
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  branches <- App.query $ St.GetBranchesByRepoA name
  allJobs <- App.query $ St.GetJobsByRepoA repo.name
  pure $ W.layout cfg (crumbs <> [LinkTo.Repo name]) $ do
    viewRepo cfg.linkTo repo branches allJobs

updateHandler :: RepoName -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
updateHandler name = do
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  allBranches <- liftIO $ Git.remoteBranches repo.cloneUrl
  App.update $ St.SetRepoBranchesA repo.name allBranches
  pure $ addHeader True "Ok"

deleteHandler :: RepoName -> Eff App.AppServantStack (Headers '[HXRedirect] Text)
deleteHandler name = do
  cfg <- ask
  App.query (St.GetRepoByNameA name) >>= \case
    Just _repo -> do
      App.update $ St.DeleteRepoByNameA name
      let redirectUrl :: String = show $ linkURI $ cfg.linkTo LinkTo.RepoListing
      pure $ addHeader (toText redirectUrl) "Ok"
    Nothing ->
      throwError err404

-- Repository header component with actions
repoPageHeader :: (LinkTo.LinkTo -> Link) -> St.Repo -> Html ()
repoPageHeader linkTo repo = do
  W.viraPageHeader_
    (toText $ toString repo.name)
    ( div_ [class_ "flex items-center justify-between"] $ do
        p_ [class_ "text-indigo-700 font-mono break-all"] $
          toHtml repo.cloneUrl
        div_ [class_ "flex items-center gap-2 ml-4"] $ do
          W.viraButton_
            W.ButtonSecondary
            [ hxPostSafe_ $ linkTo $ LinkTo.RepoUpdate repo.name
            , hxSwapS_ AfterEnd
            , title_ "Refresh branches"
            ]
            $ do
              W.viraButtonIcon_ $ toHtmlRaw Icon.refresh
              "Refresh"
          W.viraButton_
            W.ButtonDestructive
            [ hxPostSafe_ $ linkTo $ LinkTo.RepoDelete repo.name
            , hxSwapS_ AfterEnd
            , hxConfirm_ "Are you sure you want to delete this repository? This action cannot be undone."
            , title_ "Delete repository"
            ]
            $ W.viraButtonIcon_
            $ toHtmlRaw Icon.trash
    )

-- TODO: Can we use `HtmlT (ReaderT ..) ()` to avoid threading the linkTo function?
viewRepo :: (LinkTo.LinkTo -> Link) -> St.Repo -> [St.Branch] -> [St.Job] -> Html ()
viewRepo linkTo repo branches allJobs = do
  repoPageHeader linkTo repo
  repoLayout linkTo repo branches Nothing $ do
    div_ [class_ "mb-8"] $ do
      div_ [class_ "flex items-center mb-3"] $ do
        div_ [class_ "text-gray-600 w-8 h-8 mr-3 flex items-center justify-center"] $ toHtmlRaw Icon.activity
        h2_ [class_ "text-2xl font-bold text-gray-800"] "All Branches"
      div_ [class_ "h-px bg-gray-200"] mempty
    viewJobListing linkTo allJobs

-- Branch-specific view function
viewRepoBranch :: (LinkTo.LinkTo -> Link) -> St.Repo -> St.Branch -> [St.Branch] -> [St.Job] -> Html ()
viewRepoBranch linkTo repo branch branches jobs = do
  repoPageHeader linkTo repo
  repoLayout linkTo repo branches (Just branch.branchName) $ do
    -- Enhanced branch header
    div_ [class_ "mb-8"] $ do
      div_ [class_ "flex items-start justify-between mb-6"] $ do
        div_ [class_ "flex-1"] $ do
          div_ [class_ "flex items-center mb-3"] $ do
            div_ [class_ "text-gray-600 w-8 h-8 mr-3 flex items-center justify-center"] $ toHtmlRaw Icon.git_branch
            h2_ [class_ "text-2xl font-bold text-gray-800"] $
              toHtml $
                toString branch.branchName
          div_ [class_ "inline-flex items-center bg-gray-100 rounded-lg px-3 py-2 border"] $ do
            span_ [class_ "text-xs text-gray-600 font-medium mr-2"] "Latest commit:"
            div_ [class_ "text-xs text-gray-700 font-mono"] $
              JobPage.viewCommit branch.headCommit

        -- Enhanced build button
        div_ [class_ "flex-shrink-0 ml-6"] $ do
          W.viraButton_
            W.ButtonSuccess
            [ hxPostSafe_ $ linkTo $ LinkTo.Build repo.name branch.branchName
            , hxSwapS_ AfterEnd
            , class_ "transition-colors"
            ]
            $ do
              W.viraButtonIcon_ $ toHtmlRaw Icon.player_play
              "Build Branch"

      div_ [class_ "h-px bg-gray-200"] mempty

    -- Enhanced section header
    div_ [class_ "mb-6"] $ do
      div_ [class_ "flex items-center mb-3"] $ do
        div_ [class_ "text-gray-600 w-6 h-6 mr-2 flex items-center justify-center"] $ toHtmlRaw Icon.list
        h3_ [class_ "text-xl font-semibold text-gray-800"] "Build History"

    viewJobListing linkTo jobs

-- Job listing component with updated messaging
viewJobListing :: (LinkTo.LinkTo -> Link) -> [St.Job] -> Html ()
viewJobListing linkTo jobs = do
  if null jobs
    then W.viraCard_ [class_ "p-12 text-center bg-gray-50"] $ do
      h3_ [class_ "text-xl font-semibold text-gray-700 mb-2"] "No builds yet"
      div_ [class_ "inline-flex items-center text-sm text-indigo-600 font-medium"] $ do
        "Use the Build button to start a build"
    else div_ [class_ "space-y-4"] $ forM_ jobs $ \job -> do
      W.viraCard_ [class_ "p-4 hover:bg-gray-50 transition-colors"] $ do
        JobPage.viewJobHeader linkTo job

-- Repository layout component with sidebar and main content
repoLayout :: (LinkTo.LinkTo -> Link) -> St.Repo -> [St.Branch] -> Maybe BranchName -> Html () -> Html ()
repoLayout linkTo repo branches currentBranch content = do
  div_ [class_ "grid grid-cols-4 gap-8"] $ do
    -- Sidebar
    div_ [class_ "col-span-1"] $ do
      repoSidebar linkTo repo branches currentBranch

    -- Main content
    div_ [class_ "col-span-3"] $ do
      div_ [class_ "bg-white rounded-xl border border-gray-200 p-8"] $ do
        content
  where
    -- Sidebar component for repository navigation
    repoSidebar :: (LinkTo.LinkTo -> Link) -> St.Repo -> [St.Branch] -> Maybe BranchName -> Html ()
    repoSidebar linkToFunc repository branches' maybeCurrentBranch = do
      W.viraCard_ [class_ "p-6 bg-gray-50"] $ do
        div_ [class_ "mb-6"] $ do
          h3_ [class_ "text-lg font-semibold text-gray-700 mb-2"] "Branches"
          div_ [class_ "h-px bg-gray-200"] mempty

        -- Branch filter input
        div_ [class_ "mb-6"] $ do
          W.viraFilterInput_
            "[data-branch-item]"
            [placeholder_ "Filter branches...", id_ "branch-filter"]

          -- Filter results counter
          div_ [class_ "mt-2 text-xs text-gray-600", id_ "branch-count"] $
            toHtml $
              show @Text (length branches') <> " branches"

        nav_ [class_ "space-y-3 max-h-96 overflow-y-auto", id_ "branch-navigation"] $ do
          -- All branches entry with design system colors
          let allBranchesActive = isNothing maybeCurrentBranch
              allBranchesClass =
                if allBranchesActive
                  then "flex items-center p-4 rounded-lg bg-indigo-50 border border-indigo-200"
                  else "flex items-center p-4 rounded-lg hover:bg-gray-50 transition-colors"
          a_ [href_ $ show $ linkURI $ linkToFunc $ LinkTo.Repo repository.name, class_ allBranchesClass, data_ "branch-item" "all-branches"] $ do
            div_ [class_ "flex-shrink-0 mr-3"] $ do
              div_ [class_ "w-5 h-5 flex items-center justify-center"] $ toHtmlRaw Icon.list
            div_ [class_ "flex-1"] $ do
              div_ [class_ $ "font-semibold " <> if allBranchesActive then "text-indigo-700" else "text-gray-700"] "All Branches"
              div_ [class_ "text-xs text-gray-600 mt-1"] $
                toHtml $
                  show @Text (length branches') <> " branches"

          -- Individual branches with enhanced styling
          forM_ branches' $ \branch -> do
            let url = linkURI $ linkToFunc $ LinkTo.RepoBranch repository.name branch.branchName
                isCurrentBranch = maybeCurrentBranch == Just branch.branchName
                branchClass =
                  if isCurrentBranch
                    then "flex items-center p-4 rounded-lg bg-indigo-50 border border-indigo-200"
                    else "flex items-center p-4 rounded-lg hover:bg-gray-50 transition-colors"
                branchNameText = toText $ toString branch.branchName
            a_ [href_ $ show url, class_ branchClass, data_ "branch-item" branchNameText] $ do
              div_ [class_ "flex-shrink-0 mr-3"] $ do
                div_ [class_ "w-5 h-5 flex items-center justify-center"] $ toHtmlRaw Icon.git_branch
              div_ [class_ "flex-1 min-w-0"] $ do
                div_ [class_ $ "text-sm font-medium truncate " <> if isCurrentBranch then "text-indigo-700" else "text-gray-700"] $
                  toHtml $
                    toString branch.branchName
                div_ [class_ "text-xs text-gray-600 mt-1 font-mono"] $
                  JobPage.viewCommit branch.headCommit
