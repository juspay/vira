{-# LANGUAGE OverloadedRecordDot #-}

-- | Nix tool-specific logic
module Vira.Environment.Tool.Tools.Nix (
  NixStatus (..),
  getToolData,
  viewToolStatus,
) where

import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import DevourFlake (devourFlakePath)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import Lucid (HtmlT, class_, code_, details_, div_, span_, summary_, toHtml)
import System.Nix.Config (NixConfig (..), nixConfigShow)
import System.Nix.Core (nix)
import System.Nix.Version (NixVersion (..), getVersion)
import Vira.Environment.Tool.Type.ToolData (ToolData (..))

-- | Status type for Nix tool (version + configuration)
data NixStatus = NixStatus
  { version :: NixVersion
  , config :: NixConfig
  }
  deriving stock (Show)

-- | Get Nix tool data with metadata and runtime info
getToolData ::
  ( Process :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  Eff es (ToolData (Either Text NixStatus))
getToolData = do
  versionResult <- getVersion
  configResult <- runErrorNoCallStack nixConfigShow

  let status = case (versionResult, configResult) of
        (Right ver, Right cfg) -> Right $ NixStatus ver cfg
        (Left err, _) -> Left $ "Version error: " <> err
        (_, Left err) -> Left $ "Config error: " <> err

  pure
    ToolData
      { name = "Nix"
      , url = "https://nixos.org"
      , binPaths = toText nix :| [toText devourFlakePath]
      , status
      }

-- | Display Nix status with version and configuration
viewToolStatus :: (Monad m) => Either Text NixStatus -> HtmlT m ()
viewToolStatus = \case
  Left err ->
    div_ [class_ "text-sm text-red-600 dark:text-red-400"] $
      "Error: " <> toHtml err
  Right (NixStatus (NixVersion ver) cfg) -> do
    -- Version
    div_ [class_ "text-sm text-gray-700 dark:text-gray-300 mb-3"] $
      "Version: " <> toHtml ver

    -- Build settings
    div_ [class_ "text-xs space-y-1"] $ do
      div_ $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "Max Jobs: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $
          toHtml @Text $
            show cfg.maxJobs <> if cfg.maxJobs == 0 then " (auto)" else ""

      div_ $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "Cores: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $
          toHtml @Text $
            show cfg.cores <> if cfg.cores == 0 then " (all)" else ""

      div_ $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "System: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $ toHtml cfg.system

    -- Extra platforms
    unless (null cfg.extraPlatforms) $ do
      div_ [class_ "mt-2 text-xs"] $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "Extra Platforms: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $
          toHtml $
            T.intercalate ", " cfg.extraPlatforms

    -- Experimental features
    unless (null cfg.experimentalFeatures) $ do
      div_ [class_ "mt-2 text-xs"] $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "Experimental: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $
          toHtml $
            T.intercalate ", " cfg.experimentalFeatures

    -- Binary caches (collapsible)
    unless (null cfg.substituters) $ do
      details_ [class_ "mt-3"] $ do
        summary_ [class_ "text-xs text-gray-500 dark:text-gray-400 cursor-pointer hover:text-gray-700 dark:hover:text-gray-300"] $
          "Binary caches (" <> toHtml @Text (show $ length cfg.substituters) <> ")"
        div_ [class_ "mt-2 space-y-2"] $ do
          forM_ (zip cfg.substituters (cfg.trustedPublicKeys <> repeat "")) $ \(sub, key) -> do
            div_ [class_ "text-xs"] $ do
              div_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $ toHtml sub
              unless (T.null key) $
                div_ [class_ "text-gray-500 dark:text-gray-400 font-mono text-[10px] ml-2"] $
                  "🔑 " <> toHtml (T.take 40 key) <> if T.length key > 40 then "..." else ""

    -- Raw config (collapsible)
    details_ [class_ "mt-3"] $ do
      summary_ [class_ "text-xs text-gray-500 dark:text-gray-400 cursor-pointer hover:text-gray-700 dark:hover:text-gray-300"] $
        "Raw configuration (" <> toHtml @Text (show $ Map.size cfg.rawConfig) <> " entries)"
      div_ [class_ "mt-2 space-y-1 max-h-64 overflow-y-auto"] $ do
        forM_ (Map.toList cfg.rawConfig) $ \(key, val) -> do
          div_ [class_ "text-[10px] font-mono"] $ do
            span_ [class_ "text-gray-600 dark:text-gray-400"] $ toHtml key
            span_ [class_ "text-gray-500 dark:text-gray-500"] " = "
            code_ [class_ "text-gray-700 dark:text-gray-300 bg-gray-100 dark:bg-gray-800 px-1 rounded"] $
              toHtml $
                if T.null val then "(empty)" else val
