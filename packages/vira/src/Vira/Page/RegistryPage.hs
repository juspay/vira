{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.RegistryPage where

import Colog (Severity (..))
import Effectful (Eff)
import GHC.Records (HasField)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (InnerHTML))
import Lucid
import Lucid.Htmx.Contrib (hxPostSafe_)
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Git (BranchName)
import Vira.Lib.Logging
import Vira.Page.BranchPage qualified as BranchPage
import Vira.Page.RepoPage qualified as RepoPage
import Vira.State.Acid qualified as St
import Vira.State.Type (Repo (..), RepoName (..), RepoSettings (..))
import Vira.Widgets.Alert qualified as W
import Vira.Widgets.Button qualified as W
import Vira.Widgets.Card qualified as W
import Vira.Widgets.Form qualified as W
import Vira.Widgets.Layout qualified as W
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (ask, asks, for_)

type FormReq a = ReqBody '[FormUrlEncoded] a
type FormResp = Headers '[HXRedirect] (Html ())

data Routes mode = Routes
  { _listing :: mode :- Get '[HTML] (Html ())
  , _repo :: mode :- Capture "name" RepoName :> NamedRoutes RepoPage.Routes
  , _branch :: mode :- Capture "repo" RepoName :> "branches" :> Capture "name" BranchName :> NamedRoutes BranchPage.Routes
  , _addRepo :: mode :- "add" :> FormReq Repo :> Post '[HTML] FormResp
  }
  deriving stock (Generic)

handlers :: App.AppState -> App.WebSettings -> Routes AsServer
handlers cfg webSettings = do
  Routes
    { _listing = App.runAppInServant cfg webSettings $ App.runAppHtml handleListing
    , _repo = RepoPage.handlers cfg webSettings
    , _branch = BranchPage.handlers cfg webSettings
    , _addRepo = App.runAppInServant cfg webSettings . handleAddRepo
    }

handleListing :: AppHtml ()
handleListing = do
  let crumbs = [LinkTo.RepoListing]
  W.layout crumbs viewRepoList

handleAddRepo :: Repo -> Eff App.AppServantStack FormResp
handleAddRepo repo = do
  App.query (St.GetRepoByNameA repo.name) >>= \case
    Just _repo -> do
      log Warning $ "Repository exists " <> toText repo.name
      -- Show error message instead of redirecting
      errorHtml <- App.runAppHtml $ do
        newRepoForm
        W.viraAlert_ W.AlertError $ do
          p_ [class_ "text-red-800 font-medium"] $ do
            "Repository "
            strong_ $ toHtml $ toString repo.name
            " already exists."
      pure $ noHeader errorHtml
    Nothing -> do
      App.update $ St.AddNewRepoA repo
      log Info $ "Added repository " <> toText repo.name
      -- Redirect to the newly created repository page
      newRepoUrl <- App.getLinkUrl $ LinkTo.Repo repo.name
      pure $ addHeader newRepoUrl "Ok"

viewRepoList :: App.AppHtml ()
viewRepoList = do
  registry <- lift $ App.query St.GetAllReposA
  W.viraSection_ [] $ do
    W.viraPageHeader_ "Repositories" $ do
      p_ [class_ "text-gray-600"] "Repositories managed by this Vira instance"

    if null registry
      then W.viraCard_ [class_ "p-12 text-center"] $ do
        div_ [class_ "text-gray-500 mb-4"] $
          div_ [class_ "w-16 h-16 mx-auto flex items-center justify-center"] $
            toHtmlRaw Icon.book_2
        h3_ [class_ "text-xl font-semibold text-gray-700 mb-2"] "No repositories yet"
        p_ [class_ "text-gray-500 mb-6"] "Add your first repository to start building and monitoring your projects"
        newRepoForm
      else do
        -- Repository grid
        div_ [class_ "grid gap-6 mb-8 md:grid-cols-2 lg:grid-cols-3"] $ do
          forM_ registry $ \repo -> do
            url <- lift $ App.getLinkUrl $ LinkTo.Repo repo.name
            W.viraNavigationCard_
              url
              (toHtml $ toString repo.name)

        -- Add new repository section
        W.viraCard_ [class_ "p-6 bg-indigo-50 border-2 border-indigo-200"] $ do
          h3_ [class_ "text-xl font-semibold text-gray-900 mb-4 flex items-center"] $ do
            div_ [class_ "w-5 h-5 mr-2 flex items-center justify-center"] $ toHtmlRaw Icon.plus
            "Add New Repository"
          newRepoForm

newRepoForm :: App.AppHtml ()
newRepoForm = do
  repoAddLink <- lift $ App.getLink LinkTo.RepoAdd
  form_ [hxPostSafe_ repoAddLink, hxSwapS_ InnerHTML, class_ "space-y-6"] $ do
    div_ [class_ "grid grid-cols-1 lg:grid-cols-2 gap-6"] $ do
      W.viraFormGroup_
        ( withFieldName @Repo @"name" $ \name ->
            W.viraLabel_ [for_ name] "Repository Name"
        )
        ( withFieldName @Repo @"name" $ \name ->
            W.viraInput_
              [ type_ "text"
              , id_ name
              , name_ name
              , placeholder_ "my-awesome-project"
              , required_ ""
              ]
        )
      W.viraFormGroup_
        ( withFieldName @Repo @"cloneUrl" $ \name ->
            W.viraLabel_ [for_ name] "Git Clone URL"
        )
        ( withFieldName @Repo @"cloneUrl" $ \name ->
            W.viraInput_
              [ type_ "url"
              , id_ name
              , name_ name
              , placeholder_ "https://github.com/user/repo.git"
              , required_ ""
              ]
        )

    -- Hidden dummy settings field (required by form structure)
    withFieldName @RepoSettings @"dummy" $ \name -> do
      W.viraInput_
        [ type_ "hidden"
        , name_ name
        , value_ ""
        ]

    div_ [class_ "flex justify-end"] $ do
      W.viraButton_ W.ButtonPrimary [type_ "submit", class_ "px-8"] $ do
        W.viraButtonIcon_ $ toHtmlRaw Icon.plus
        "Add Repository"

withFieldName ::
  forall record field a r.
  (KnownSymbol field, HasField field record a) =>
  (Text -> r) ->
  r
withFieldName k =
  let fieldName = toText (symbolVal (Proxy @field))
   in k fieldName
