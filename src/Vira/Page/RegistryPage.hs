{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.RegistryPage where

import Effectful (Eff)
import Effectful.Reader.Dynamic (ask)
import GHC.Records (HasField)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (InnerHTML))
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Logging
import Vira.Lib.HTMX (hxPostSafe_)
import Vira.Page.RepoPage qualified as RepoPage
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (Repo (..), RepoName (..), RepoSettings (..))
import Vira.Widgets qualified as W
import Prelude hiding (ask, asks, for_)

type FormReq a = ReqBody '[FormUrlEncoded] a
type FormResp = Headers '[HXRefresh] (Html ())

data Routes mode = Routes
  { _listing :: mode :- Get '[HTML] (Html ())
  , _repo :: mode :- Capture "name" RepoName :> NamedRoutes RepoPage.Routes
  , _addRepo :: mode :- "add" :> FormReq Repo :> Post '[HTML] FormResp
  , _deleteRepo :: mode :- "delete" :> FormReq RepoName :> Post '[HTML] FormResp
  }
  deriving stock (Generic)

handlers :: App.AppState -> Routes AsServer
handlers cfg = do
  Routes
    { _listing = App.runAppInServant cfg handleListing
    , _repo = RepoPage.handlers cfg
    , _addRepo = App.runAppInServant cfg . addRepoHandler
    , _deleteRepo = App.runAppInServant cfg . deleteRepoHandler
    }

handleListing :: Eff App.AppServantStack (Html ())
handleListing = do
  cfg <- ask
  samples <- App.query St.GetAllReposA
  let crumbs = [LinkTo.RepoListing]
  pure $ W.layout cfg crumbs $ do
    viewRepoList cfg.linkTo samples

addRepoHandler :: Repo -> Eff App.AppServantStack FormResp
addRepoHandler repo = do
  cfg <- ask
  App.query (St.GetRepoByNameA repo.name) >>= \case
    Just _repo -> do
      log Debug $ "Repository exists " <> toText repo.name
      pure $ addHeader False $ do
        -- Don't refresh, swap the existing form for visual feedback on the browser
        newRepoForm cfg.linkTo
        p_ "Repository exists"
    Nothing -> do
      App.update $ St.AddNewRepoA repo
      log Info $ "Added repository " <> toText repo.name
      pure $ addHeader True "Ok"

deleteRepoHandler :: RepoName -> Eff App.AppServantStack FormResp
deleteRepoHandler name = do
  App.query (St.GetRepoByNameA name) >>= \case
    Just _repo -> do
      App.update $ St.DeleteRepoByNameA name
      log Info $ "Deleted repository " <> toText name
      pure $ addHeader True "Ok"
    Nothing -> do
      log Debug $ "Repository not found " <> toText name
      pure $ addHeader True "Repository not Found"

viewRepoList :: (LinkTo.LinkTo -> Link) -> [St.Repo] -> Html ()
viewRepoList linkTo registry = do
  div_ [class_ "space-y-8"] $ do
    -- Repository listing
    div_ $ do
      h2_ [class_ "text-2xl font-semibold mb-4 text-gray-800"] "Repositories"
      div_ [class_ "space-y-3"] $ do
        -- Show existing repositories first
        forM_ registry $ \repo ->
          div_ [class_ "flex items-center justify-between p-3 bg-gray-50 rounded-md"] $ do
            div_ $ do
              let url = linkURI $ linkTo $ LinkTo.Repo repo.name
              a_ [href_ $ show url, class_ "text-blue-600 hover:text-blue-800 hover:underline"] $ do
                strong_ [class_ "text-gray-900"] $ toHtml $ toString repo.name
              br_ []
              span_ [class_ "text-sm text-gray-600"] $ toHtml repo.cloneUrl
            form_ [hxPostSafe_ $ linkTo LinkTo.RepoDelete, hxSwapS_ InnerHTML, class_ "inline"] $ do
              withFieldName @RepoName @"unRepoName" $ \name ->
                W.viraInput_ [type_ "hidden", name_ name, value_ $ toText $ toString repo.name]
              W.viraButton_ [type_ "submit", class_ "bg-red-600 hover:bg-red-700"] "Delete"

        -- Add new repository form at the end
        div_ [class_ "border-2 border-dashed border-gray-300 rounded-md p-4 bg-gray-50/50"] $ do
          details_ [class_ "group"] $ do
            summary_ [class_ "cursor-pointer text-lg font-medium text-gray-700 hover:text-gray-900 flex items-center"] $ do
              span_ [class_ "mr-2"] "+"
              span_ "Add New Repository"
            div_ [class_ "mt-4 pl-6"] $ do
              newRepoForm linkTo

newRepoForm :: (LinkTo.LinkTo -> Link) -> Html ()
newRepoForm linkTo = do
  form_ [hxPostSafe_ $ linkTo LinkTo.RepoAdd, hxSwapS_ InnerHTML, class_ "space-y-4"] $ do
    div_ $ do
      withFieldName @Repo @"name" $ \name -> do
        W.viraLabel_ [for_ name] "Name"
        W.viraInput_
          [ type_ "text"
          , name_ name
          , placeholder_ "my-repo"
          ]
    div_ $ do
      withFieldName @Repo @"cloneUrl" $ \name -> do
        W.viraLabel_ [for_ name] "Clone URL"
        W.viraInput_
          [ type_ "url"
          , name_ name
          , placeholder_ "https://github.com/user/repo.git"
          ]
    details_ [class_ "border border-gray-300 rounded-md p-4"] $ do
      summary_ [class_ "cursor-pointer text-lg font-medium mb-2"] "Settings (optional)"
      div_ $ do
        withFieldName @RepoSettings @"dummy" $ \name -> do
          W.viraLabel_ [for_ name] "Dummy"
          W.viraInput_
            [ type_ "text"
            , name_ name
            ]
    W.viraButton_ [type_ "submit"] "Add"

withFieldName ::
  forall record field a r.
  (KnownSymbol field, HasField field record a) =>
  (Text -> r) ->
  r
withFieldName k =
  let fieldName = toText (symbolVal (Proxy @field))
   in k fieldName
