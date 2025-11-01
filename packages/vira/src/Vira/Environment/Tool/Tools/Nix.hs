{-# LANGUAGE DuplicateRecordFields #-}

-- | Nix tool-specific logic
module Vira.Environment.Tool.Tools.Nix (
  NixStatus (..),
  getToolData,
  viewToolStatus,
) where

import Colog.Message (RichMessage)
import Data.Text qualified as T
import DevourFlake (devourFlakePath)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import Lucid (HtmlT, class_, details_, div_, span_, summary_, title_, toHtml)
import System.Nix.Config qualified as Nix
import System.Nix.Core (nix)
import System.Nix.Version (NixVersion (..), getVersion)
import Vira.Environment.Tool.Type.ToolData (ToolData (..))

-- | Status type for Nix tool ('NixVersion' + 'System.Nix.Config.NixConfig')
data NixStatus = NixStatus
  { version :: NixVersion
  -- ^ 'System.Nix.Version.NixVersion' information
  , config :: Nix.NixConfig
  -- ^ 'System.Nix.Config.NixConfig' from @nix show-config@
  }
  deriving stock (Show)

-- | Get Nix 'ToolData' with metadata and runtime info
getToolData ::
  ( Process :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  Eff es (ToolData (Either Text NixStatus))
getToolData = do
  versionResult <- getVersion
  configResult <- runErrorNoCallStack Nix.nixConfigShow

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
    let Nix.NixConfig
          { builders = Nix.NixConfigField {value = buildersVal, description = buildersDesc}
          , maxJobs = Nix.NixConfigField {value = maxJobsVal, description = maxJobsDesc}
          , cores = Nix.NixConfigField {value = coresVal, description = coresDesc}
          , system = Nix.NixConfigField {value = systemVal, description = systemDesc}
          , extraPlatforms = Nix.NixConfigField {value = extraPlatformsVal, description = extraPlatformsDesc}
          , experimentalFeatures = Nix.NixConfigField {value = experimentalFeaturesVal, description = experimentalFeaturesDesc}
          , substituters = Nix.NixConfigField {value = substitutersVal, description = substitutersDesc}
          , trustedPublicKeys = Nix.NixConfigField {value = trustedKeysVal, description = trustedKeysDesc}
          } = cfg

    -- Version
    div_ [class_ "text-sm text-gray-700 dark:text-gray-300 mb-3"] $
      "Version: " <> toHtml ver

    -- Build settings
    div_ [class_ "text-xs space-y-1"] $ do
      div_ [title_ maxJobsDesc] $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "Max Jobs: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $
          toHtml @Text $
            show maxJobsVal <> if maxJobsVal == 0 then " (auto)" else ""

      div_ [title_ coresDesc] $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "Cores: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $
          toHtml @Text $
            show coresVal <> if coresVal == 0 then " (all)" else ""

      div_ [title_ systemDesc] $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "System: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $ toHtml @Text $ toText $ toString systemVal

      -- Builders
      div_ [title_ buildersDesc] $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "Builders: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $ case buildersVal of
          Nix.BuildersEmpty -> "none"
          Nix.BuildersFile fp -> toHtml @Text $ "@" <> toText fp
          Nix.BuildersList bs -> toHtml @Text $ show (length bs) <> " inline"

    -- Extra platforms
    unless (null extraPlatformsVal) $ do
      div_ [class_ "mt-2 text-xs", title_ extraPlatformsDesc] $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "Extra Platforms: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $
          toHtml $
            T.intercalate ", " (map (toText . toString) extraPlatformsVal)

    -- Experimental features
    unless (null experimentalFeaturesVal) $ do
      div_ [class_ "mt-2 text-xs", title_ experimentalFeaturesDesc] $ do
        span_ [class_ "text-gray-500 dark:text-gray-400"] "Experimental: "
        span_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $
          toHtml $
            T.intercalate ", " experimentalFeaturesVal

    -- Binary caches (collapsible)
    unless (null substitutersVal) $ do
      details_ [class_ "mt-3", title_ substitutersDesc] $ do
        summary_ [class_ "text-xs text-gray-500 dark:text-gray-400 cursor-pointer hover:text-gray-700 dark:hover:text-gray-300"] $
          "Binary caches (" <> toHtml @Text (show $ length substitutersVal) <> ")"
        div_ [class_ "mt-2 space-y-2"] $ do
          forM_ (zip substitutersVal (trustedKeysVal <> repeat "")) $ \(sub, key) -> do
            div_ [class_ "text-xs"] $ do
              div_ [class_ "text-gray-700 dark:text-gray-300 font-mono"] $ toHtml sub
              unless (T.null key) $
                div_ [class_ "text-gray-500 dark:text-gray-400 font-mono text-[10px] ml-2", title_ trustedKeysDesc] $
                  "🔑 " <> toHtml (T.take 40 key) <> if T.length key > 40 then "..." else ""
