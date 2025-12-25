{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Structured logging for Vira pipeline messages

'ViraLog' messages are written as JSON (one object per line) to @output.log.json@.
Web UI renders them with 'Colog.Severity'-specific styling (emoji + colored background).
-}
module Vira.CI.Log (
  ViraLog (..),
  encodeViraLog,
  encodeViraLogJson,
  decodeViraLog,
  renderViraLogCLI,
) where

import Colog (Severity (..))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), decode, encode)
import Data.List (lookup)
import Data.Text qualified as T
import Effectful.Colog.Simple (LogContext (..), severityEmoji)
import Lucid (ToHtml (..), br_, class_, span_, toHtml)
import System.Console.ANSI (
  Color (..),
  ColorIntensity (..),
  ConsoleLayer (..),
  SGR (..),
  setSGRCode,
 )

-- Standalone JSON instances for Severity using Show/Read
instance ToJSON Severity where
  toJSON = toJSON . (show :: Severity -> String)

instance FromJSON Severity where
  parseJSON v = do
    s :: String <- parseJSON v
    case readMaybe s of
      Just sev -> pure sev
      Nothing -> fail $ "Unknown severity: " <> s

-- JSON instances for LogContext
instance ToJSON LogContext where
  toJSON (LogContext pairs) = toJSON pairs

instance FromJSON LogContext where
  parseJSON v = LogContext <$> parseJSON v

-- | Vira structured log entry
data ViraLog = ViraLog
  { level :: Severity
  , message :: Text
  , context :: LogContext
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Encode ViraLog to JSON with viralog: prefix (legacy)
encodeViraLog :: ViraLog -> Text
encodeViraLog viraLog = "viralog:" <> encodeViraLogJson viraLog

-- | Encode ViraLog to pure JSON (no prefix)
encodeViraLogJson :: ViraLog -> Text
encodeViraLogJson viraLog = decodeUtf8 (encode viraLog)

{- | Decode ViraLog from a line.
Tries pure JSON first, then falls back to viralog: prefix for backwards compatibility.
Returns Left with original line if not a valid ViraLog entry (for raw printing).
-}
decodeViraLog :: Text -> Either Text ViraLog
decodeViraLog line =
  -- Try pure JSON first
  case decode (encodeUtf8 line) of
    Just viraLog -> Right viraLog
    Nothing ->
      -- Fall back to viralog: prefix for backwards compatibility
      case T.stripPrefix "viralog:" line of
        Nothing -> Left line
        Just jsonStr ->
          case decode (encodeUtf8 jsonStr) of
            Nothing -> Left line
            Just viraLog -> Right viraLog

{- | Render ViraLog for CLI with ANSI colors

Renders viralog entries with emoji and colored text matching the web UI styling.
Displays context key-value pairs if present.
-}
renderViraLogCLI :: ViraLog -> Text
renderViraLogCLI vlog =
  let emoji = severityEmoji vlog.level
      (intensity, clr) = case vlog.level of
        Debug -> (Dull, White)
        Info -> (Vivid, Cyan)
        Warning -> (Vivid, Yellow)
        Error -> (Vivid, Red)
      colorCode = setSGRCode [SetColor Foreground intensity clr]
      resetCode = setSGRCode [Reset]
      LogContext ctx = vlog.context
      contextStr =
        if null ctx
          then ""
          else " " <> toText (setSGRCode [SetColor Foreground Dull White]) <> "{" <> T.intercalate ", " (map renderPair ctx) <> "}" <> toText resetCode
      renderPair (k, v) = k <> "=" <> v
   in toText colorCode <> emoji <> "  " <> vlog.message <> toText resetCode <> contextStr

{- | Render ViraLog for web UI with TailwindCSS

Renders viralog entries with:
- System icon prefix (🍎 darwin, 🐧 linux) if system is in context
- Severity emoji and colored message
- All context key-value pairs shown after message
-}
instance ToHtml ViraLog where
  toHtml viraLog =
    let emoji = severityEmoji viraLog.level
        textClass = case viraLog.level of
          Debug -> "text-slate-400 dark:text-slate-500"
          Info -> "text-cyan-400 dark:text-cyan-500"
          Warning -> "text-amber-400 dark:text-amber-500"
          Error -> "text-rose-400 dark:text-rose-500"
        LogContext ctx = viraLog.context
        -- Extract system for icon, remove from display context
        mSystem = lookup "system" ctx
        systemIcon = case mSystem of
          Just sys
            | "darwin" `T.isInfixOf` sys -> "🍎 "
            | "linux" `T.isInfixOf` sys -> "🐧 "
            | otherwise -> "❓ " -- Unknown system
          Nothing -> ""
     in span_ [class_ textClass] $ do
          -- System icon prefix
          unless (T.null systemIcon) $
            span_ [class_ "mr-1"] $
              toHtml systemIcon
          -- Severity emoji and message
          toHtml emoji
          toHtml (" " :: Text)
          toHtml viraLog.message
          -- All context as-is
          unless (null ctx) $ do
            toHtml (" " :: Text)
            span_ [class_ "text-slate-500 dark:text-slate-600 text-xs"] $ do
              toHtml ("{" :: Text)
              forM_ (intersperse Nothing $ map Just ctx) $ \case
                Nothing -> toHtml (", " :: Text)
                Just (k, v) -> toHtml $ k <> "=" <> v
              toHtml ("}" :: Text)
          br_ []
  toHtmlRaw = toHtml
