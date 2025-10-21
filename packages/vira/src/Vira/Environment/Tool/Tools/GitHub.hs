-- | GitHub tool-specific logic
module Vira.Environment.Tool.Tools.GitHub (
  getToolData,
  viewToolStatus,
  GitHubSuggestion (..),
  authStatusToSuggestion,
) where

import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import GH.Auth.Status (AuthStatus (..))
import GH.Auth.Status qualified as GH
import GH.Core qualified as GH
import GH.Signoff qualified as GH
import Lucid (HtmlT, ToHtml (..), class_, div_, p_, strong_, toHtml)
import Text.Show qualified as TS
import Vira.Environment.Tool.Type.ToolData (ToolData (..))
import Vira.Web.Widgets.Alert (AlertType (..), viraAlertWithTitle_, viraAlert_)
import Vira.Web.Widgets.Code qualified as W

-- | Suggestions for fixing GitHub CLI configuration issues
data GitHubSuggestion = GhAuthLoginSuggestion
  { bin :: FilePath
  , command :: Text
  }
  deriving stock (Eq)

instance TS.Show GitHubSuggestion where
  show GhAuthLoginSuggestion {bin, command} =
    toString $ "GH=" <> toText bin <> "\n$GH " <> command

-- | Get GitHub tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData GH.AuthStatus)
getToolData = do
  info <- liftIO GH.checkAuthStatus
  pure
    ToolData
      { name = "GitHub CLI"
      , url = "https://cli.github.com"
      , binPaths = toText GH.ghBin :| [toText GH.ghSignoffBin]
      , status = info
      }

-- | Convert an AuthStatus to a suggestion for fixing it
authStatusToSuggestion :: AuthStatus -> Maybe GitHubSuggestion
authStatusToSuggestion = \case
  Authenticated {} -> Nothing
  NotAuthenticated ->
    Just
      GhAuthLoginSuggestion
        { bin = GH.ghBin
        , command = "auth login"
        }

-- | ToHtml instance for rendering suggestions in the Tools Page
instance ToHtml GitHubSuggestion where
  toHtmlRaw = toHtml
  toHtml suggestion = do
    div_ [class_ "mt-2"] $ do
      p_ [class_ "text-sm text-red-700 dark:text-red-300 mb-1"] "Run:"
      W.viraCodeCopyable_ $ show @Text suggestion

-- | View GitHub tool status
viewToolStatus :: (Monad m) => AuthStatus -> HtmlT m ()
viewToolStatus status = do
  div_ [class_ "mb-3"] $ do
    case status of
      Authenticated {host, login, scopes} -> do
        viraAlert_ AlertSuccess $ do
          p_ [class_ "text-green-800 dark:text-green-200 font-semibold mb-1"] $ do
            "Authenticated as "
            strong_ $ toHtml login
            " on "
            strong_ $ toHtml host
          p_ [class_ "text-green-700 dark:text-green-300 text-xs"] $ do
            "Scopes: "
            toHtml $ T.intercalate ", " scopes
      NotAuthenticated -> viraAlertWithTitle_ AlertError "Not authenticated" $ do
        "Please authenticate to use GitHub CLI."
        forM_ (authStatusToSuggestion NotAuthenticated) toHtml
