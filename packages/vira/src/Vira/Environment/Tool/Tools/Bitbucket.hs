{-# LANGUAGE OverloadedRecordDot #-}

-- | Bitbucket tool-specific logic
module Vira.Environment.Tool.Tools.Bitbucket (
  getToolData,
  viewToolStatus,
  BitbucketSuggestion (..),
  mkBitbucketSuggestion,
  authStatusToSuggestion,
) where

import BB.Command.Auth.Status (checkAuthStatus)
import BB.Config (ServerConfig)
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

-- | Create a BitbucketSuggestion
mkBitbucketSuggestion :: Text -> BitbucketSuggestion
mkBitbucketSuggestion url =
  BbAuthSuggestion
    { bitbucketUrl = url
    }

instance TS.Show BitbucketSuggestion where
  show BbAuthSuggestion {bitbucketUrl} =
    toString $ toText bbBin <> " auth login " <> bitbucketUrl

-- | Get Bitbucket tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData (Either Text (Map ServerEndpoint ServerConfig)))
getToolData = do
  serversOrErr <- liftIO checkAuthStatus
  pure
    ToolData
      { name = "Bitbucket CLI"
      , url = "https://github.com/juspay/vira/tree/main/packages/bitbucket"
      , binPaths = toText bbBin :| []
      , status = serversOrErr
      }

-- | Convert server map to a suggestion for fixing it
authStatusToSuggestion :: Map ServerEndpoint ServerConfig -> Maybe BitbucketSuggestion
authStatusToSuggestion servers
  | Map.null servers = Just $ mkBitbucketSuggestion "<your bitbucket domain>"
  | otherwise = Nothing

-- | ToHtml instance for rendering suggestions in the Tools Page
instance ToHtml BitbucketSuggestion where
  toHtmlRaw = toHtml
  toHtml suggestion = do
    div_ [class_ "mt-2"] $ do
      W.viraCodeBlockCopyable Nothing $ show @Text suggestion

-- | View Bitbucket tool status
viewToolStatus :: (Monad m) => Either Text (Map ServerEndpoint ServerConfig) -> HtmlT m ()
viewToolStatus serversOrErr = do
  div_ [class_ "mb-3"] $ do
    case serversOrErr of
      Left err -> viraAlertWithTitle_ AlertError "Config error" $ do
        toHtml $ "Failed to load config: " <> err
      Right servers ->
        if Map.null servers
          then viraAlertWithTitle_ AlertError "Not authenticated" $ do
            "Please authenticate to use Bitbucket CLI."
            forM_ (authStatusToSuggestion servers) toHtml
          else do
            viraAlert_ AlertSuccess $ do
              p_ [class_ "text-green-800 dark:text-green-200 font-semibold mb-1"] $ do
                "Bitbucket CLI authenticated"
              p_ [class_ "text-green-700 dark:text-green-300 text-xs mb-2"] $ do
                "Configured servers:"
              ul_ [class_ "text-green-700 dark:text-green-300 text-xs list-disc list-inside"] $ do
                forM_ (Map.keys servers) $ \endpoint -> do
                  li_ $ toHtml endpoint.host
