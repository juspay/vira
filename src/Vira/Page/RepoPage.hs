{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.RepoPage where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Dynamic (ask)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Lucid (hxPostSafe_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (AfterEnd))
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo (LinkTo (RepoUpdate))
import Vira.App.LinkTo qualified as LinkTo
import Vira.Lib.Git qualified as Git
import Vira.Page.JobPage qualified as JobPage
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type
import Vira.Widgets qualified as W
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _update :: mode :- "fetch" :> Post '[HTML] (Headers '[HXRefresh] Text)
  }
  deriving stock (Generic)

crumbs :: [LinkTo]
crumbs = [LinkTo.RepoListing]

handlers :: App.AppState -> RepoName -> Routes AsServer
handlers cfg name = do
  Routes
    { _view = App.runAppInServant cfg $ viewHandler name
    , _update = App.runAppInServant cfg $ updateHandler name
    }

viewHandler :: RepoName -> Eff App.AppServantStack (Html ())
viewHandler name = do
  cfg <- ask
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  branches <- App.query $ St.GetBranchesByRepoA name
  xs <- forM branches $ \branch -> do
    jobs <- App.query $ St.GetJobsByBranchA repo.name branch.branchName
    pure (branch, jobs)
  pure
    $ W.layout
      cfg.linkTo
      (toHtml . toString $ name)
      (crumbs <> [LinkTo.Repo name])
    $ do
      viewRepo cfg.linkTo repo xs

updateHandler :: RepoName -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
updateHandler name = do
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  branches <- liftIO $ Git.remoteBranches repo.cloneUrl
  App.update $ St.SetRepoBranchesA repo.name branches
  pure $ addHeader True "Ok"

-- TODO: Can we use `HtmlT (ReaderT ..) ()` to avoid threading the linkTo function?
viewRepo :: (LinkTo.LinkTo -> Link) -> St.Repo -> [(St.Branch, [St.Job])] -> Html ()
viewRepo linkTo repo branches = do
  W.viraButton_
    [ hxPostSafe_ $ linkTo $ RepoUpdate repo.name
    , hxSwapS_ AfterEnd
    ]
    "Refresh repo"
  div_ $ do
    p_ "To clone this repo"
    pre_ [class_ "bg-black text-white"] $ code_ $ toHtml $ "git clone " <> repo.cloneUrl
    h2_ [class_ "text-3xl font-bold my-8"] "Branches"
    div_ [class_ "my-8"] $ do
      forM_ branches $ \(branch, jobs) -> do
        h2_ [class_ "text-2xl py-2 my-4 border-b-2"] $ code_ $ toHtml $ toString branch.branchName
        "Head Commit: " <> JobPage.viewCommit branch.headCommit
        div_ $
          W.viraButton_
            [ hxPostSafe_ $ linkTo $ LinkTo.Build repo.name branch.branchName
            , hxSwapS_ AfterEnd
            ]
            "Build"
        ul_ $ forM_ jobs $ \job -> do
          li_ [class_ "my-4 py-2"] $ do
            JobPage.viewJob linkTo job
