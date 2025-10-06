-- | GitHub tool-specific logic
module Vira.Tool.Tools.GitHub (
  getToolData,
  viewToolStatus,
  GitHubSuggestion (..),
  authStatusToSuggestion,
  suggestionToText,
) where

import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import GH.Auth.Status (AuthStatus (..))
import GH.Auth.Status qualified as GH
import GH.Core qualified as GH
import GH.Signoff qualified as GH
import Lucid (HtmlT, ToHtml (..), class_, div_, p_, strong_, toHtml)
import Vira.Tool.Type.ToolData (ToolData (..))
import Vira.Widgets.Alert (AlertType (..), viraAlert_)
import Vira.Widgets.Code qualified as W

-- | Suggestions for fixing GitHub CLI configuration issues
data GitHubSuggestion = GhAuthLoginSuggestion
  { bin :: FilePath
  , command :: Text
  }
  deriving stock (Show, Eq)

-- | Get GitHub tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData GH.AuthStatus)
getToolData = do
  info <- liftIO GH.checkAuthStatus
  pure
    ToolData
      { name = "GitHub CLI"
      , description = "GitHub command line tool for various operations"
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

-- | Convert suggestion to text for CI logs
suggestionToText :: GitHubSuggestion -> Text
suggestionToText GhAuthLoginSuggestion {bin, command} =
  "GH=" <> toText bin <> "\n$GH " <> command

-- | ToHtml instance for rendering suggestions in the Tools Page
instance ToHtml GitHubSuggestion where
  toHtmlRaw = toHtml
  toHtml suggestion = do
    div_ [class_ "mt-2"] $ do
      p_ [class_ "text-sm text-red-700 dark:text-red-300 mb-1"] "Run:"
      W.viraCodeCopyable_ $ suggestionToText suggestion

-- | View GitHub tool status
viewToolStatus :: (Monad m) => AuthStatus -> HtmlT m ()
viewToolStatus status = do
  div_ [class_ "mb-3"] $ do
    case status of
      Authenticated {host, login, scopes} -> do
        viraAlert_ AlertSuccess $ do
          p_ [class_ "text-green-800 font-semibold mb-1"] $ do
            "✓ Authenticated as "
            strong_ $ toHtml login
            " on "
            strong_ $ toHtml host
          p_ [class_ "text-green-700 text-xs"] $ do
            "Scopes: "
            toHtml $ T.intercalate ", " scopes
      NotAuthenticated -> do
        viraAlert_ AlertError $ do
          p_ [class_ "text-red-800 mb-1"] "✗ Not authenticated"
          p_ [class_ "text-red-700 text-sm"] "Please authenticate to use GitHub CLI."
          forM_ (authStatusToSuggestion NotAuthenticated) toHtml
