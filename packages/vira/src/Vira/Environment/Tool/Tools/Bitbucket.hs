{-# LANGUAGE OverloadedRecordDot #-}

-- | Bitbucket tool-specific logic
module Vira.Environment.Tool.Tools.Bitbucket (
  getToolData,
  viewToolStatus,
  BitbucketSuggestion (..),
  authStatusToSuggestion,
) where

import BB.Auth.Status (AuthStatus (..))
import BB.Auth.Status qualified as BB
import Bitbucket.API.V1.Core (ServerEndpoint (..))
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Lucid (HtmlT, ToHtml (..), class_, div_, li_, p_, toHtml, ul_)
import Text.Show qualified as TS
import Vira.Environment.Tool.Tools.Bitbucket.CLI (bbBin)
import Vira.Environment.Tool.Type.ToolData (ToolData (..))
import Vira.Web.Widgets.Alert (AlertType (..), viraAlertWithTitle_, viraAlert_)
import Vira.Web.Widgets.Code qualified as W

-- | Suggestions for fixing Bitbucket CLI configuration issues
newtype BitbucketSuggestion = BbAuthSuggestion
  { bitbucketUrl :: Text
  }
  deriving stock (Eq)

instance TS.Show BitbucketSuggestion where
  show BbAuthSuggestion {bitbucketUrl} =
    toString $ toText bbBin <> " auth --url " <> bitbucketUrl

-- | Get Bitbucket tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData BB.AuthStatus)
getToolData = do
  info <- liftIO BB.checkAuthStatus
  pure
    ToolData
      { name = "Bitbucket CLI"
      , url = "https://github.com/juspay/vira/tree/main/packages/bitbucket"
      , binPaths = toText bbBin :| []
      , status = info
      }

-- | Convert an AuthStatus to a suggestion for fixing it
authStatusToSuggestion :: AuthStatus -> Maybe BitbucketSuggestion
authStatusToSuggestion = \case
  Authenticated {} -> Nothing
  NotAuthenticated ->
    Just
      BbAuthSuggestion
        { bitbucketUrl = "https://bitbucket.example.com"
        }

-- | ToHtml instance for rendering suggestions in the Tools Page
instance ToHtml BitbucketSuggestion where
  toHtmlRaw = toHtml
  toHtml suggestion = do
    div_ [class_ "mt-2"] $ do
      p_ [class_ "text-sm text-red-700 dark:text-red-300 mb-1"] "Run:"
      W.viraCodeBlockCopyable Nothing $ show @Text suggestion

-- | View Bitbucket tool status
viewToolStatus :: (Monad m) => AuthStatus -> HtmlT m ()
viewToolStatus status = do
  div_ [class_ "mb-3"] $ do
    case status of
      Authenticated {servers} -> do
        viraAlert_ AlertSuccess $ do
          p_ [class_ "text-green-800 dark:text-green-200 font-semibold mb-1"] $ do
            "Bitbucket CLI authenticated"
          p_ [class_ "text-green-700 dark:text-green-300 text-xs mb-2"] $ do
            "Configured servers:"
          ul_ [class_ "text-green-700 dark:text-green-300 text-xs list-disc list-inside"] $ do
            forM_ (Map.keys servers) $ \endpoint -> do
              li_ $ toHtml endpoint.host
      NotAuthenticated -> viraAlertWithTitle_ AlertError "Not authenticated" $ do
        "Please authenticate to use Bitbucket CLI."
        forM_ (authStatusToSuggestion NotAuthenticated) toHtml
