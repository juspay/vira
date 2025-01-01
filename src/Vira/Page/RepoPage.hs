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
  , _job :: mode :- "job" :> NamedRoutes JobPage.Routes
  }
  deriving stock (Generic)

crumbs :: [LinkTo]
crumbs = [LinkTo.RepoListing]

handlers :: App.AppState -> RepoName -> Routes AsServer
handlers cfg name = do
  Routes
    { _view = App.runAppInServant cfg $ viewHandler name
    , _update = App.runAppInServant cfg $ updateHandler name
    , _job = JobPage.handlers cfg name
    }

viewHandler :: RepoName -> Eff App.AppServantStack (Html ())
viewHandler name = do
  cfg <- ask
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  branches <- App.query $ St.GetBranchesByRepoA name
  pure
    $ W.layout
      cfg.linkTo
      (toHtml . toString $ name)
      (crumbs <> [LinkTo.Repo name])
    $ do
      viewRepo cfg.linkTo repo branches

updateHandler :: RepoName -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
updateHandler name = do
  repo <- App.query (St.GetRepoByNameA name) >>= maybe (throwError err404) pure
  branches <- liftIO $ Git.remoteBranches repo.cloneUrl
  App.update $ St.SetRepoBranchesA repo.name branches
  pure $ addHeader True "Ok"

-- TODO: Can we use `HtmlT (ReaderT ..) ()` to avoid threading the linkTo function?
viewRepo :: (LinkTo.LinkTo -> Link) -> St.Repo -> [St.Branch] -> Html ()
viewRepo linkTo repo branches = do
  W.viraButton_
    [ hxPostSafe_ $ linkTo $ RepoUpdate repo.name
    , hxSwapS_ AfterEnd
    ]
    "Refresh repo"
  div_ $ do
    p_ "To clone this repo"
    pre_ [class_ "bg-black text-white"] $ code_ $ toHtml $ "git clone " <> repo.cloneUrl
    h2_ [class_ "text-2xl font-bold"] "Branches"
    table_ $ do
      forM_ branches $ \branch -> do
        tr_ $ do
          td_ $ b_ $ toHtml . toString $ branch.branchName
          td_ $ do
            let branchJobs = linkTo $ LinkTo.RepoBranchJobs repo.name branch.branchName
            a_ [href_ $ show $ linkURI branchJobs] $
              pre_ $
                toHtml . toString $
                  branch.headCommit
          -- Add a button to trigger building of this commit
          td_ $
            W.viraButton_
              [ hxPostSafe_ $ linkTo $ LinkTo.Build repo.name branch.branchName
              , hxSwapS_ AfterEnd
              ]
              "Build"
