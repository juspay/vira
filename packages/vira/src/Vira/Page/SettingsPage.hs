{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Vira's global settings page
module Vira.Page.SettingsPage (
  Routes (..),
  handlers,
)
where

import Colog (Severity (..))
import Effectful (Eff)
import GHC.Records (HasField)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (InnerHTML))
import Lucid
import Lucid.Htmx.Contrib (hxConfirm_, hxPostSafe_)
import Servant
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Servant (HTML)
import Vira.Lib.Attic (AtticServer (..))
import Vira.Lib.Logging
import Vira.State.Acid qualified as St
import Vira.State.Type (AtticSettings (..), CachixSettings (..))
import Vira.Widgets.Alert qualified as W
import Vira.Widgets.Button qualified as W
import Vira.Widgets.Card qualified as W
import Vira.Widgets.Form qualified as W
import Vira.Widgets.Layout qualified as W
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
  App.runVHtml $ W.layout [LinkTo.Settings] viewSettings

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

viewSettings :: App.VHtml ()
viewSettings = do
  mCachix <- lift $ App.query St.GetCachixSettingsA
  mAttic <- lift $ App.query St.GetAtticSettingsA
  W.viraSection_ [] $ do
    W.viraPageHeader_ "Settings" $ do
      p_ [class_ "text-gray-600"] "Configure build cache providers and CI/CD integrations"

    div_ [class_ "grid gap-8 lg:grid-cols-2"] $ do
      -- Cachix Configuration
      W.viraCard_ [class_ "p-6"] $ do
        div_ [class_ "flex items-center mb-6"] $ do
          span_ [class_ "h-8 w-8 mr-3 bg-blue-100 rounded-lg flex items-center justify-center text-blue-600 font-bold"] "C"
          h3_ [class_ "text-xl font-bold text-gray-900"] "Cachix Configuration"

        case mCachix of
          Just cachix -> do
            W.viraAlert_ W.AlertSuccess $ do
              p_ [class_ "text-green-800"] $ do
                "Connected to cache: "
                strong_ $ toHtml cachix.cachixName
            W.viraDivider_
          Nothing -> do
            W.viraAlert_ W.AlertInfo $ do
              p_ [class_ "text-blue-800"] "Configure Cachix to enable build caching"
            W.viraDivider_

        cachixForm mCachix

      -- Attic Configuration
      W.viraCard_ [class_ "p-6"] $ do
        div_ [class_ "flex items-center mb-6"] $ do
          span_ [class_ "h-8 w-8 mr-3 bg-purple-100 rounded-lg flex items-center justify-center text-purple-600 font-bold"] "A"
          h3_ [class_ "text-xl font-bold text-gray-900"] "Attic Configuration"

        case mAttic of
          Just attic -> do
            W.viraAlert_ W.AlertSuccess $ do
              p_ [class_ "text-green-800"] $ do
                "Connected to "
                strong_ $ toHtml attic.atticServer.serverName
                " ("
                toHtml $ toText attic.atticCacheName
                ")"
            W.viraDivider_
          Nothing -> do
            W.viraAlert_ W.AlertInfo $ do
              p_ [class_ "text-blue-800"] "Configure Attic for distributed build caching"
            W.viraDivider_

        atticForm mAttic
  where
    cachixForm :: Maybe CachixSettings -> App.VHtml ()
    cachixForm mCachixSettings = do
      updateCachixLink <- lift $ App.getLink LinkTo.SettingsUpdateCachix
      form_ [id_ "cachix-update", hxPostSafe_ updateCachixLink, hxSwapS_ InnerHTML, class_ "space-y-6"] $ do
        W.viraFormGroup_
          ( withFieldName @CachixSettings @"cachixName" $ \name ->
              W.viraLabel_ [for_ name] "Cache Name"
          )
          ( withFieldName @CachixSettings @"cachixName" $ \name ->
              W.viraInput_
                [ type_ "text"
                , name_ name
                , value_ $ maybe "" (.cachixName) mCachixSettings
                , placeholder_ "my-cache"
                ]
          )

        W.viraFormGroup_
          ( withFieldName @CachixSettings @"authToken" $ \name ->
              W.viraLabel_ [for_ name] "Auth Token"
          )
          ( withFieldName @CachixSettings @"authToken" $ \name ->
              W.viraInput_ $
                [ type_ "password"
                , name_ name
                , placeholder_ $ maybe "Enter your Cachix auth token" (const "••••••••••••") mCachixSettings
                ]
                  <> maybe [] (const []) mCachixSettings
          )

        div_ [class_ "flex items-center gap-3 pt-4"] $ do
          W.viraButton_ W.ButtonPrimary [type_ "submit", form_ "cachix-update"] $ do
            case mCachixSettings of
              Nothing -> "Connect Cachix"
              Just _ -> "Update Settings"

      -- Disconnect form outside the main form to avoid nesting
      whenJust mCachixSettings $ \_ -> do
        deleteCachixLink <- lift $ App.getLink LinkTo.SettingsDeleteCachix
        form_ [hxPostSafe_ deleteCachixLink, hxSwapS_ InnerHTML, hxConfirm_ "Are you sure you want to disconnect Cachix? This action cannot be undone.", class_ "mt-3"] $ do
          W.viraButton_ W.ButtonDestructive [type_ "submit"] "Disconnect"

    atticForm :: Maybe AtticSettings -> App.VHtml ()
    atticForm mAtticSettings = do
      updateAtticLink <- lift $ App.getLink LinkTo.SettingsUpdateAttic
      form_ [id_ "attic-update", hxPostSafe_ updateAtticLink, hxSwapS_ InnerHTML, class_ "space-y-6"] $ do
        div_ [class_ "grid gap-4 lg:grid-cols-2"] $ do
          W.viraFormGroup_
            ( withFieldName @AtticServer @"serverName" $ \name ->
                W.viraLabel_ [for_ name] "Server Name"
            )
            ( withFieldName @AtticServer @"serverName" $ \name ->
                W.viraInput_
                  [ type_ "text"
                  , name_ name
                  , value_ $ maybe "" ((.serverName) . (.atticServer)) mAtticSettings
                  , placeholder_ "my-attic"
                  ]
            )

          W.viraFormGroup_
            ( withFieldName @AtticServer @"serverUrl" $ \name ->
                W.viraLabel_ [for_ name] "Server URL"
            )
            ( withFieldName @AtticServer @"serverUrl" $ \name ->
                W.viraInput_
                  [ type_ "url"
                  , name_ name
                  , value_ $ maybe "" ((.serverUrl) . (.atticServer)) mAtticSettings
                  , placeholder_ "https://attic.example.com"
                  ]
            )

        div_ [class_ "grid gap-4 lg:grid-cols-2"] $ do
          W.viraFormGroup_
            ( withFieldName @AtticSettings @"atticCacheName" $ \name ->
                W.viraLabel_ [for_ name] "Cache Name"
            )
            ( withFieldName @AtticSettings @"atticCacheName" $ \name ->
                W.viraInput_
                  [ type_ "text"
                  , name_ name
                  , value_ $ maybe "" (toText . (.atticCacheName)) mAtticSettings
                  , placeholder_ "cache-name"
                  ]
            )

          W.viraFormGroup_
            ( withFieldName @AtticSettings @"atticToken" $ \name ->
                W.viraLabel_ [for_ name] "Token"
            )
            ( withFieldName @AtticSettings @"atticToken" $ \name ->
                W.viraInput_ $
                  [ type_ "password"
                  , name_ name
                  , placeholder_ $ maybe "Enter your Attic token" (const "••••••••••••") mAtticSettings
                  ]
                    <> maybe [] (const []) mAtticSettings
            )

        div_ [class_ "flex items-center gap-3 pt-4"] $ do
          W.viraButton_ W.ButtonPrimary [type_ "submit", form_ "attic-update"] $ do
            case mAtticSettings of
              Nothing -> "Connect Attic"
              Just _ -> "Update Settings"

      -- Disconnect form outside the main form to avoid nesting
      whenJust mAtticSettings $ \_ -> do
        deleteAtticLink <- lift $ App.getLink LinkTo.SettingsDeleteAttic
        form_ [hxPostSafe_ deleteAtticLink, hxSwapS_ InnerHTML, hxConfirm_ "Are you sure you want to disconnect Attic? This action cannot be undone.", class_ "mt-3"] $ do
          W.viraButton_ W.ButtonDestructive [type_ "submit"] "Disconnect"

withFieldName ::
  forall record field a r.
  (KnownSymbol field, HasField field record a) =>
  (Text -> r) ->
  r
withFieldName k =
  let fieldName = toText (symbolVal (Proxy @field))
   in k fieldName
