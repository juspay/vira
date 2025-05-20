{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.BranchPage where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Dynamic (ask)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Swap (Swap (AfterEnd))
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Git (BranchName)
import Vira.Lib.HTMX (hxPostSafe_)
import Vira.Page.JobPage qualified as JobPage
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type
import Vira.Widgets qualified as W
import Prelude hiding (ask, asks)

newtype Routes mode = Routes {_view :: mode :- Get '[HTML] (Html ())}
  deriving stock (Generic)

handlers :: App.AppState -> RepoName -> BranchName -> Routes AsServer
handlers cfg repo branch = Routes {_view = App.runAppInServant cfg $ viewHandler repo branch}

viewHandler :: RepoName -> BranchName -> Eff App.AppServantStack (Html ())
viewHandler repoName branchName = do
  repo <- App.query (St.GetRepoByNameA repoName) >>= maybe (throwError err404) pure
  branch <- App.query (St.GetBranchByNameA repoName branchName) >>= maybe (throwError err404) pure
  jobs <- App.query $ St.GetJobsByBranchA repoName branchName
  cfg <- ask
  let crumbs = [LinkTo.RepoListing, LinkTo.Repo repoName, LinkTo.RepoBranch repoName branchName]
  pure $ W.layout cfg crumbs $ do
    viewRepoBranch cfg.linkTo repo (branch, jobs)

-- TODO: Can we use `HtmlT (ReaderT ..) ()` to avoid threading the linkTo function?
viewRepoBranch :: (LinkTo.LinkTo -> Link) -> St.Repo -> (St.Branch, [St.Job]) -> Html ()
viewRepoBranch linkTo repo (branch, jobs) = do
  div_ [class_ ""] $ do
    h2_ [class_ "text-2xl py-2 my-4 border-b-2 flex items-start flex-col"] $ do
      div_ $ toHtml $ toString branch.branchName
      div_ $ JobPage.viewCommit branch.headCommit
    div_ $
      W.viraButton_
        [ hxPostSafe_ $ linkTo $ LinkTo.Build repo.name branch.branchName
        , hxSwapS_ AfterEnd
        ]
        "Build"
    ul_ $ forM_ jobs $ \job -> do
      li_ [class_ "my-2 py-1"] $ do
        JobPage.viewJobHeader linkTo job
