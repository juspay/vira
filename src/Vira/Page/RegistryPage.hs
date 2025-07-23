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
type FormResp = Headers '[HXRedirect] (Html ())

data Routes mode = Routes
  { _listing :: mode :- Get '[HTML] (Html ())
  , _repo :: mode :- Capture "name" RepoName :> NamedRoutes RepoPage.Routes
  , _addRepo :: mode :- "add" :> FormReq Repo :> Post '[HTML] FormResp
  }
  deriving stock (Generic)

handlers :: App.AppState -> Routes AsServer
handlers cfg = do
  Routes
    { _listing = App.runAppInServant cfg handleListing
    , _repo = RepoPage.handlers cfg
    , _addRepo = App.runAppInServant cfg . addRepoHandler
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
      -- Redirect to the existing repository page
      let existingRepoUrl :: String = show $ linkURI $ cfg.linkTo $ LinkTo.Repo repo.name
      pure $ addHeader (toText existingRepoUrl) $ do
        newRepoForm cfg.linkTo
        p_ "Repository exists"
    Nothing -> do
      App.update $ St.AddNewRepoA repo
      log Info $ "Added repository " <> toText repo.name
      -- Redirect to the newly created repository page
      let newRepoUrl :: String = show $ linkURI $ cfg.linkTo $ LinkTo.Repo repo.name
      pure $ addHeader (toText newRepoUrl) "Ok"

viewRepoList :: (LinkTo.LinkTo -> Link) -> [St.Repo] -> Html ()
viewRepoList linkTo registry = do
  div_ [class_ "space-y-6 max-w-4xl"] $ do
    -- Repository listing
    div_ $ do
      h2_ [class_ "text-2xl font-semibold mb-6 text-gray-800"] "Repositories"
      if null registry
        then div_ [class_ "text-center py-8 text-gray-500"] $ do
          p_ "No repositories yet."
        else div_ [class_ "space-y-2"] $ do
          -- Show existing repositories as clean list
          forM_ registry $ \repo -> do
            let url = linkURI $ linkTo $ LinkTo.Repo repo.name
            a_ [href_ $ show url, class_ "block p-4 bg-white border border-gray-200 rounded-lg hover:bg-gray-50 hover:border-gray-300 transition-colors"] $ do
              div_ [class_ "flex items-center justify-between"] $ do
                h3_ [class_ "text-lg font-medium text-gray-900 hover:text-blue-600"] $
                  toHtml $
                    toString repo.name
                span_ [class_ "text-xs text-gray-400 font-mono truncate ml-4 max-w-md"] $
                  toHtml repo.cloneUrl

    -- Add new repository section - more prominent
    div_ [class_ "mt-8 pt-6 border-t border-gray-200 mb-8"] $ do
      div_ [class_ "bg-gray-50 border border-gray-300 rounded-lg p-6"] $ do
        h3_ [class_ "text-lg font-medium text-gray-900 mb-4"] "Add Repository"
        newRepoForm linkTo

newRepoForm :: (LinkTo.LinkTo -> Link) -> Html ()
newRepoForm linkTo = do
  form_ [hxPostSafe_ $ linkTo LinkTo.RepoAdd, hxSwapS_ InnerHTML, class_ "space-y-4"] $ do
    div_ [class_ "grid grid-cols-1 md:grid-cols-2 gap-4"] $ do
      div_ $ do
        withFieldName @Repo @"name" $ \name -> do
          W.viraLabel_ [for_ name] "Repository Name"
          W.viraInput_
            [ type_ "text"
            , name_ name
            , placeholder_ "my-project"
            , class_ "w-full"
            ]
      div_ $ do
        withFieldName @Repo @"cloneUrl" $ \name -> do
          W.viraLabel_ [for_ name] "Git Clone URL"
          W.viraInput_
            [ type_ "url"
            , name_ name
            , placeholder_ "https://github.com/user/repo.git"
            , class_ "w-full"
            ]
    -- Hidden dummy settings field (required by form structure)
    withFieldName @RepoSettings @"dummy" $ \name -> do
      W.viraInput_
        [ type_ "hidden"
        , name_ name
        , value_ ""
        ]
    div_ [class_ "flex justify-end"] $ do
      W.viraButton_ [type_ "submit", class_ "bg-blue-600 hover:bg-blue-700"] "Add Repository"

withFieldName ::
  forall record field a r.
  (KnownSymbol field, HasField field record a) =>
  (Text -> r) ->
  r
withFieldName k =
  let fieldName = toText (symbolVal (Proxy @field))
   in k fieldName
