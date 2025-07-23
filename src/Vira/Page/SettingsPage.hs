{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.SettingsPage (
  Routes (..),
  handlers,
)
where

import Effectful (Eff)
import Effectful.Reader.Dynamic (ask)
import GHC.Records (HasField)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (InnerHTML))
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Logging
import Vira.Lib.Attic (AtticServer (..))
import Vira.Lib.HTMX (hxPostSafe_)
import Vira.State.Acid qualified as St
import Vira.State.Type (AtticSettings (..), CachixSettings (..))
import Vira.Widgets qualified as W
import Prelude hiding (ask, for_)

type FormReq a = ReqBody '[FormUrlEncoded] a
type FormResp = Headers '[HXRefresh] (Html ())

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _updateCachix :: mode :- "cachix" :> FormReq CachixSettings :> Post '[HTML] FormResp
  , _deleteCachix :: mode :- "cachix" :> "delete" :> Post '[HTML] FormResp
  , _updateAttic :: mode :- "attic" :> FormReq AtticSettings :> Post '[HTML] FormResp
  , _deleteAttic :: mode :- "attic" :> "delete" :> Post '[HTML] FormResp
  }
  deriving stock (Generic)

handlers :: App.AppState -> Routes AsServer
handlers cfg =
  Routes
    { _view = App.runAppInServant cfg viewHandler
    , _updateCachix = App.runAppInServant cfg . updateCachixHandler
    , _deleteCachix = App.runAppInServant cfg deleteCachixHandler
    , _updateAttic = App.runAppInServant cfg . updateAtticHandler
    , _deleteAttic = App.runAppInServant cfg deleteAtticHandler
    }

viewHandler :: Eff App.AppServantStack (Html ())
viewHandler = do
  cfg <- ask
  mCachix <- App.query St.GetCachixSettingsA
  mAttic <- App.query St.GetAtticSettingsA
  pure $ W.layout cfg [LinkTo.Settings] $ viewSettings cfg.linkTo mCachix mAttic

updateCachixHandler :: CachixSettings -> Eff App.AppServantStack FormResp
updateCachixHandler settings = do
  App.update $ St.SetCachixSettingsA (Just settings)
  log Info $ "Updated cachix settings for " <> settings.cachixName
  pure $ addHeader True "Ok"

deleteCachixHandler :: Eff App.AppServantStack FormResp
deleteCachixHandler = do
  App.update $ St.SetCachixSettingsA Nothing
  log Info "Deleted cachix settings"
  pure $ addHeader True "Ok"

updateAtticHandler :: AtticSettings -> Eff App.AppServantStack FormResp
updateAtticHandler settings = do
  App.update $ St.SetAtticSettingsA (Just settings)
  log Info $ "Updated attic settings for " <> settings.atticServer.serverName <> ":" <> toText settings.atticCacheName
  pure $ addHeader True "Ok"

deleteAtticHandler :: Eff App.AppServantStack FormResp
deleteAtticHandler = do
  App.update $ St.SetAtticSettingsA Nothing
  log Info "Deleted attic settings"
  pure $ addHeader True "Ok"

viewSettings :: (LinkTo.LinkTo -> Link) -> Maybe CachixSettings -> Maybe AtticSettings -> Html ()
viewSettings linkTo mCachix mAttic =
  div_ [class_ "space-y-8"] $ do
    section_ [class_ "bg-white border-2 border-gray-300 rounded-xl shadow-md p-6"] cachixForm
    section_ [class_ "bg-white border-2 border-gray-300 rounded-xl shadow-md p-6"] atticForm
  where
    cachixForm :: Html ()
    cachixForm = do
      h2_ [class_ "text-2xl font-semibold mb-4 text-gray-800"] "Cachix"
      form_ [id_ "cachix-update", hxPostSafe_ $ linkTo LinkTo.SettingsCachix, hxSwapS_ InnerHTML, class_ "space-y-4"] $ do
        div_ $ do
          withFieldName @CachixSettings @"cachixName" $ \name -> do
            W.viraLabel_ [for_ name] "Cache Name"
            W.viraInput_
              [ type_ "text"
              , name_ name
              , value_ $ maybe "" (.cachixName) mCachix
              ]
        div_ $ do
          withFieldName @CachixSettings @"authToken" $ \name -> do
            W.viraLabel_ [for_ name] "Auth Token"
            W.viraInput_ $
              [ type_ "text"
              , name_ name
              ]
                <> maybe [] (const [placeholder_ "Hidden for security reasons"]) mCachix

      div_ [class_ "flex items-center space-x-2 mt-4"] $ do
        W.viraButton_ [type_ "submit", form_ "cachix-update"] "Update"
        whenJust mCachix $ \_ ->
          form_ [hxPostSafe_ $ linkTo LinkTo.SettingsDeleteCachix, hxSwapS_ InnerHTML] $ do
            W.viraButton_ [type_ "submit", class_ "bg-red-600 hover:bg-red-700"] "Delete"

    atticForm :: Html ()
    atticForm = do
      h2_ [class_ "text-2xl font-semibold mb-4 text-gray-800"] "Attic"
      form_ [id_ "attic-update", hxPostSafe_ $ linkTo LinkTo.SettingsAttic, hxSwapS_ InnerHTML, class_ "space-y-4"] $ do
        div_ $ do
          withFieldName @AtticServer @"serverName" $ \name -> do
            W.viraLabel_ [for_ name] "Server Name"
            W.viraInput_
              [ type_ "text"
              , name_ name
              , value_ $ maybe "" ((.serverName) . (.atticServer)) mAttic
              ]
        div_ $ do
          withFieldName @AtticServer @"serverUrl" $ \name -> do
            W.viraLabel_ [for_ name] "Server URL"
            W.viraInput_
              [ type_ "url"
              , name_ name
              , value_ $ maybe "" ((.serverUrl) . (.atticServer)) mAttic
              ]
        div_ $ do
          withFieldName @AtticSettings @"atticCacheName" $ \name -> do
            W.viraLabel_ [for_ name] "Cache Name"
            W.viraInput_
              [ type_ "text"
              , name_ name
              , value_ $ maybe "" (toText . (.atticCacheName)) mAttic
              ]
        div_ $ do
          withFieldName @AtticSettings @"atticToken" $ \name -> do
            W.viraLabel_ [for_ name] "Token"
            W.viraInput_ $
              [ type_ "text"
              , name_ name
              ]
                <> maybe [] (const [placeholder_ "Hidden for security reasons"]) mAttic
      div_ [class_ "flex items-center space-x-2 mt-4"] $ do
        W.viraButton_ [type_ "submit", form_ "attic-update"] "Update"
        whenJust mAttic $ \_ ->
          form_ [hxPostSafe_ $ linkTo LinkTo.SettingsDeleteAttic, hxSwapS_ InnerHTML] $ do
            W.viraButton_ [type_ "submit", class_ "bg-red-600 hover:bg-red-700"] "Delete"

withFieldName ::
  forall record field a r.
  (KnownSymbol field, HasField field record a) =>
  (Text -> r) ->
  r
withFieldName k =
  let fieldName = toText (symbolVal (Proxy @field))
   in k fieldName
