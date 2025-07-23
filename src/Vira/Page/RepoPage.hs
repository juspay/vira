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
  div_ $ do
    div_ [class_ "flex items-center justify-between mb-4"] $ do
      pre_ [class_ "rounded py-2 flex-1"] $ code_ $ toHtml repo.cloneUrl
      div_ [class_ "flex gap-2 ml-4"] $ do
        W.viraButton_
          [ hxPostSafe_ $ linkTo $ LinkTo.RepoUpdate repo.name
          , hxSwapS_ AfterEnd
          , class_ "bg-blue-600 hover:bg-blue-700"
          ]
          "Refresh branches"
        W.viraButton_
          [ hxPostSafe_ $ linkTo $ LinkTo.RepoDelete repo.name
          , hxSwapS_ AfterEnd
          , class_ "bg-red-600 hover:bg-red-700"
          , hxConfirm_ "Are you sure you want to delete this repository? This action cannot be undone."
          ]
          "Delete Repository"
    div_ [class_ "space-y-8"] $ do
      forM_ branches $ \(branch, jobs) -> do
        section_ [id_ (toText $ "branch-" <> toString branch.branchName), class_ "bg-white border-2 border-gray-300 rounded-xl shadow-md hover:shadow-lg hover:border-blue-400 transition-all duration-300 p-4 my-6"] $ do
          let url = linkURI $ linkTo $ LinkTo.RepoBranch repo.name branch.branchName
          h2_ [class_ "text-2xl font-semibold mb-3 border-b-2 border-blue-100 pb-2"] $ do
            a_ [href_ $ show url, class_ "text-blue-600 hover:text-blue-800 hover:underline"] $ do
              toHtml $ toString branch.branchName
          div_ [class_ "mb-4 text-gray-600"] $ do
            JobPage.viewCommit branch.headCommit
          div_ [class_ "mb-4"] $
            W.viraButton_
              [ hxPostSafe_ $ linkTo $ LinkTo.Build repo.name branch.branchName
              , hxSwapS_ AfterEnd
              ]
              "Build"
          BranchPage.viewJobListing linkTo jobs
          div_ [class_ "text-right"] $ do
            a_ [href_ $ show url, class_ "text-blue-600 hover:text-blue-800 text-sm font-medium hover:underline"] "View all jobs →"
