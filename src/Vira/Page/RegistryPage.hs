{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.RegistryPage where

import Effectful (Eff)
import Effectful.Reader.Dynamic (ask)
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo qualified as LinkTo
import Vira.Page.RepoPage qualified as RepoPage
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (RepoName)
import Vira.Widgets qualified as W
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { _listing :: mode :- Get '[HTML] (Html ())
  , _repo :: mode :- Capture "name" RepoName :> NamedRoutes RepoPage.Routes
  }
  deriving stock (Generic)

handlers :: App.AppState -> Routes AsServer
handlers cfg = do
  Routes
    { _listing = App.runAppInServant cfg handleListing
    , _repo = RepoPage.handlers cfg
    }

handleListing :: Eff App.AppServantStack (Html ())
handleListing = do
  cfg <- ask
  samples <- App.query St.GetAllReposA
  let crumbs = [LinkTo.RepoListing]
  pure $ W.layout cfg.linkTo "Repositories" crumbs $ do
    viewRepoList cfg.linkTo samples

viewRepoList :: (LinkTo.LinkTo -> Link) -> [St.Repo] -> Html ()
viewRepoList linkTo registry = do
  ul_ [class_ "max-w-md divide-y divide-blue-200"] $ do
    forM_ registry $ \repo -> do
      li_ [class_ "py-3 hover:bg-blue-50 font-bold text-xl"] $ do
        let url = linkURI $ linkTo $ LinkTo.Repo repo.name
        a_ [href_ $ show url] $ toHtml . toString $ repo.name
