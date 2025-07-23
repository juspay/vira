{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.RepoPage where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Dynamic (ask)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (AfterEnd))
import Lucid
import Servant hiding (throwError)
import Servant.API ((:>))
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Git (BranchName)
import Vira.Lib.Git qualified as Git
import Vira.Lib.HTMX (hxConfirm_, hxPostSafe_)
import Vira.Page.BranchPage qualified as BranchPage
import Vira.Page.JobPage qualified as JobPage
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type
import Vira.Widgets qualified as W
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _update :: mode :- "fetch" :> Post '[HTML] (Headers '[HXRefresh] Text)
  , _delete :: mode :- "delete" :> Post '[HTML] (Headers '[HXRedirect] Text)
  , _branch :: mode :- "branches" Servant.API.:> Capture "name" BranchName :> NamedRoutes BranchPage.Routes
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
    , _branch = BranchPage.handlers cfg name
    }

viewHandler :: RepoName -> Eff App.AppServantStack (Html ())
viewHandler name = do
  cfg <- ask
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  branches <- App.query $ St.GetBranchesByRepoA name
  xs <- forM branches $ \branch -> do
    jobs <- App.query $ St.GetJobsByBranchA repo.name branch.branchName
    let recentJobs = take 5 jobs
    pure (branch, recentJobs)
  pure $ W.layout cfg (crumbs <> [LinkTo.Repo name]) $ do
    viewRepo cfg.linkTo repo xs

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

-- TODO: Can we use `HtmlT (ReaderT ..) ()` to avoid threading the linkTo function?
viewRepo :: (LinkTo.LinkTo -> Link) -> St.Repo -> [(St.Branch, [St.Job])] -> Html ()
viewRepo linkTo repo branches = do
  W.viraSection_ [] $ do
    -- Repository header with improved styling
    W.viraCard_ [class_ "p-6 mb-8"] $ do
      div_ [class_ "flex items-center justify-between"] $ do
        div_ [class_ "flex-1"] $ do
          W.viraPageHeader_ (toText $ toString repo.name) $ do
            W.viraCodeBlock_ (toText repo.cloneUrl)
        div_ [class_ "flex gap-3 ml-6"] $ do
          W.viraButton_
            [ hxPostSafe_ $ linkTo $ LinkTo.RepoUpdate repo.name
            , hxSwapS_ AfterEnd
            , class_ "bg-blue-600 hover:bg-blue-700 focus:ring-blue-500"
            ]
            "🔄 Refresh Branches"
          W.viraButton_
            [ hxPostSafe_ $ linkTo $ LinkTo.RepoDelete repo.name
            , hxSwapS_ AfterEnd
            , class_ "bg-red-600 hover:bg-red-700 focus:ring-red-500"
            , hxConfirm_ "Are you sure you want to delete this repository? This action cannot be undone."
            ]
            "🗑️ Delete Repository"

    -- Branches section
    div_ [class_ "space-y-6"] $ do
      forM_ branches $ \(branch, jobs) -> do
        let url = linkURI $ linkTo $ LinkTo.RepoBranch repo.name branch.branchName
        W.viraCard_ [id_ (toText $ "branch-" <> toString branch.branchName), class_ "overflow-hidden hover:shadow-xl transition-all duration-300"] $ do
          -- Branch header
          div_ [class_ "bg-gradient-to-r from-indigo-50 to-blue-50 p-6 border-b border-gray-100"] $ do
            div_ [class_ "flex items-center justify-between"] $ do
              div_ $ do
                h2_ [class_ "text-2xl font-bold text-gray-900 mb-2"] $ do
                  a_ [href_ $ show url, class_ "text-indigo-600 hover:text-indigo-800 transition-colors hover:underline"] $ do
                    toHtml $ toString branch.branchName
                div_ [class_ "text-gray-600"] $ do
                  JobPage.viewCommit branch.headCommit
              div_ $ do
                W.viraButton_
                  [ hxPostSafe_ $ linkTo $ LinkTo.Build repo.name branch.branchName
                  , hxSwapS_ AfterEnd
                  , class_ "bg-green-600 hover:bg-green-700 focus:ring-green-500"
                  ]
                  "🚀 Build"

          -- Jobs listing
          div_ [class_ "p-6"] $ do
            BranchPage.viewJobListing linkTo jobs
            W.viraDivider_
            div_ [class_ "flex justify-end"] $ do
              a_ [href_ $ show url, class_ "inline-flex items-center text-indigo-600 hover:text-indigo-800 font-medium transition-colors group"] $ do
                "View all jobs"
                span_ [class_ "ml-2 transform transition-transform group-hover:translate-x-1"] "→"
