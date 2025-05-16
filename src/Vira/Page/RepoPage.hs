{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.RepoPage where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Dynamic (ask, asks)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (AfterEnd))
import Lucid
import Servant hiding (throwError)
import Servant.API ((:>))
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo.Type (LinkTo (RepoUpdate))
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Git (BranchName)
import Vira.Lib.Git qualified as Git
import Vira.Lib.HTMX (hxPostSafe_)
import Vira.Page.BranchJobsPage qualified as BranchJobsPage
import Vira.Page.JobPage qualified as JobPage
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type
import Vira.Widgets qualified as W
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _update :: mode :- "fetch" :> Post '[HTML] (Headers '[HXRefresh] Text)
  , _branchJobs :: mode :- "branches" Servant.API.:> Capture "name" BranchName :> NamedRoutes BranchJobsPage.Routes
  }
  deriving stock (Generic)

crumbs :: [LinkTo]
crumbs = [LinkTo.RepoListing]

handlers :: App.AppState -> RepoName -> Routes AsServer
handlers cfg name = do
  Routes
    { _view = App.runAppInServant cfg $ viewHandler name
    , _update = App.runAppInServant cfg $ updateHandler name
    , _branchJobs = BranchJobsPage.handlers cfg name
    }

viewHandler :: RepoName -> Eff App.AppServantStack (Html ())
viewHandler name = do
  cfg <- ask
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  branches <- App.query $ St.GetBranchesByRepoA name
  jobs <- App.query $ St.GetJobsByRepoA name
  pure $ W.layout cfg (crumbs <> [LinkTo.Repo name]) $ do
    viewRepo cfg.linkTo repo branches (take 5 jobs)

updateHandler :: RepoName -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
updateHandler name = do
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  allBranches <- liftIO $ Git.remoteBranches repo.cloneUrl
  branchWhitelist <- asks $ App.branchWhitelist . App.repo . App.settings
  let branches = Map.filterWithKey (\k _ -> (toText . toString) k `Set.member` branchWhitelist) allBranches
  App.update $ St.SetRepoBranchesA repo.name branches
  pure $ addHeader True "Ok"

-- TODO: Can we use `HtmlT (ReaderT ..) ()` to avoid threading the linkTo function?
viewRepo :: (LinkTo.LinkTo -> Link) -> St.Repo -> [St.Branch] -> [St.Job] -> Html ()
viewRepo linkTo repo branches jobs = do
  div_ $ do
    pre_ [class_ "rounded py-2 my-4"] $ code_ $ toHtml repo.cloneUrl
    W.viraButton_
      [ hxPostSafe_ $ linkTo $ RepoUpdate repo.name
      , hxSwapS_ AfterEnd
      ]
      "Refresh branches"
    ul_ [class_ "max-w-md divide-y divide-blue-200"] $ do
      forM_ branches $ \branch -> do
        li_ [class_ "py-3 hover:bg-blue-50 font-bold text-xl"] $ do
          let url = linkURI $ linkTo $ LinkTo.BranchJobs repo.name branch.branchName
          a_ [href_ $ show url] $ toHtml $ toString branch.branchName
    ul_ $ forM_ jobs $ \job -> do
      li_ [class_ "my-2 py-1"] $ do
        JobPage.viewJobHeader linkTo job
