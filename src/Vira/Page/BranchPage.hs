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
import Vira.App.LinkTo.Type (LinkTo (RepoUpdate))
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
    viewRepoBranch cfg.linkTo repo branch jobs

-- TODO: Can we use `HtmlT (ReaderT ..) ()` to avoid threading the linkTo function?
viewRepoBranch :: (LinkTo.LinkTo -> Link) -> St.Repo -> St.Branch -> [St.Job] -> Html ()
viewRepoBranch linkTo repo branch jobs = do
  W.viraSection_ [] $ do
    -- Branch header
    W.viraCard_ [class_ "p-6 mb-8"] $ do
      W.viraPageHeader_ (toText $ toString branch.branchName) $ do
        JobPage.viewCommit branch.headCommit

      div_ [class_ "flex gap-3 mt-6"] $ do
        -- TODO: Replace this with parent UX flow
        -- cf. https://github.com/juspay/vira/issues/47#issuecomment-3014376804
        W.viraButton_
          [ hxPostSafe_ $ linkTo $ RepoUpdate repo.name
          , hxSwapS_ AfterEnd
          , class_ "bg-blue-600 hover:bg-blue-700 focus:ring-blue-500"
          ]
          "🔄 Refresh Branches"
        W.viraButton_
          [ hxPostSafe_ $ linkTo $ LinkTo.Build repo.name branch.branchName
          , hxSwapS_ AfterEnd
          , class_ "bg-green-600 hover:bg-green-700 focus:ring-green-500"
          ]
          "🚀 Build"

    -- Jobs listing
    W.viraCard_ [class_ "p-6"] $ do
      h3_ [class_ "text-xl font-semibold text-gray-900 mb-6"] "Build History"
      viewJobListing linkTo jobs

viewJobListing :: (LinkTo.LinkTo -> Link) -> [St.Job] -> Html ()
viewJobListing linkTo jobs = do
  if null jobs
    then div_ [class_ "text-center py-12 text-gray-500"] $ do
      p_ [class_ "text-lg"] "No builds yet"
      p_ [class_ "text-sm mt-2"] "Start your first build using the button above"
    else div_ [class_ "divide-y divide-gray-200"] $ forM_ jobs $ \job -> do
      div_ [class_ "py-4 hover:bg-gray-50 transition-colors duration-150"] $ do
        JobPage.viewJobHeader linkTo job
