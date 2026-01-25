{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Web.Pages.RegistryPage where

import Effectful (Eff)
import Effectful.Colog.Simple
import Effectful.Git (BranchName, RepoName (..))
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
import Vira.App qualified as App
import Vira.Refresh qualified as Refresh
import Vira.Refresh.Type (RefreshPriority (Now))
import Vira.State.Acid qualified as St
import Vira.State.Type (Repo (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLink, getLinkUrl, runAppHtml)
import Vira.Web.Pages.BranchPage qualified as BranchPage
import Vira.Web.Pages.RepoPage qualified as RepoPage
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Alert qualified as W
import Vira.Web.Widgets.Button qualified as W
import Vira.Web.Widgets.Card qualified as W
import Vira.Web.Widgets.Form qualified as W
import Vira.Web.Widgets.Layout qualified as W
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

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> App.WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings = do
  Routes
    { _listing = Web.runAppInServant globalSettings viraRuntimeState webSettings $ runAppHtml handleListing
    , _repo = RepoPage.handlers globalSettings viraRuntimeState webSettings
    , _branch = BranchPage.handlers globalSettings viraRuntimeState webSettings
    , _addRepo = Web.runAppInServant globalSettings viraRuntimeState webSettings . handleAddRepo
    }

handleListing :: AppHtml ()
handleListing = do
  let crumbs = [LinkTo.RepoListing]
  W.layout crumbs viewRepoList

handleAddRepo :: Repo -> Eff Web.AppServantStack FormResp
handleAddRepo repo = do
  App.query (St.GetRepoByNameA repo.name) >>= \case
    Just _repo -> do
      log Warning $ "Repository exists " <> toText repo.name
      -- Show error message instead of redirecting
      errorHtml <- runAppHtml $ do
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
      -- Schedule immediate refresh for new repo
      Refresh.scheduleRepoRefresh (one repo.name) Now
      -- Redirect to the newly created repository page
      newRepoUrl <- getLinkUrl $ LinkTo.Repo repo.name
      pure $ addHeader newRepoUrl "Ok"

viewRepoList :: AppHtml ()
viewRepoList = do
  registry <- lift $ App.query St.GetAllReposA
  W.viraSection_ [] $ do
    W.viraPageHeader_ "Repositories" $ do
      p_ [class_ "text-gray-600 dark:text-gray-300"] "Repositories managed by this Vira instance"

    if null registry
      then W.viraCard_ [class_ "p-12 text-center"] $ do
        div_ [class_ "text-gray-500 dark:text-gray-400 mb-4"] $
          div_ [class_ "w-16 h-16 mx-auto flex items-center justify-center"] $
            toHtmlRaw Icon.book_2
        h3_ [class_ "text-xl font-bold text-gray-700 dark:text-gray-200 mb-2"] "No repositories yet"
        p_ [class_ "text-gray-500 dark:text-gray-400 mb-6"] "Add your first repository to start building and monitoring your projects"
        newRepoForm
      else do
        -- Repository grid
        div_ [class_ "grid gap-6 mb-8 md:grid-cols-2 lg:grid-cols-3"] $ do
          forM_ registry $ \repo -> do
            url <- lift $ getLinkUrl $ LinkTo.Repo repo.name
            W.viraNavigationCard_
              url
              (toHtml $ toString repo.name)

        -- Add new repository section
        W.viraCard_ [class_ "p-6 bg-indigo-50 dark:bg-indigo-900/20 border-2 border-indigo-200 dark:border-indigo-800"] $ do
          h3_ [class_ "text-xl font-bold text-gray-900 dark:text-gray-100 mb-4 flex items-center"] $ do
            div_ [class_ "w-5 h-5 mr-2 flex items-center justify-center"] $ toHtmlRaw Icon.plus
            "Add New Repository"
          newRepoForm

newRepoForm :: AppHtml ()
newRepoForm = do
  repoAddLink <- lift $ getLink LinkTo.RepoAdd
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
              [ id_ name
              , name_ name
              , placeholder_ "https://github.com/user/repo.git"
              , required_ ""
              ]
        )

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
