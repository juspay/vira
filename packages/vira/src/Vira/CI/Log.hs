{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Structured logging for Vira pipeline messages

Vira logs are written as JSON with "viralog:" prefix to output.log,
distinguishing them from build tool output. Web UI renders them with
severity-specific styling (emoji + colored background).
-}
module Vira.CI.Log (
  ViraLog (..),
  encodeViraLog,
  decodeViraLog,
  renderViraLogCLI,
) where

import Colog (Severity (..))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), decode, encode)
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

-- | Encode ViraLog to JSON with viralog: prefix
encodeViraLog :: ViraLog -> Text
encodeViraLog viraLog = "viralog:" <> decodeUtf8 (encode viraLog)

{- | Decode ViraLog from a line, stripping viralog: prefix and parsing JSON
Returns Left with original line if not a valid viralog entry (for raw printing)
-}
decodeViraLog :: Text -> Either Text ViraLog
decodeViraLog line =
  case T.stripPrefix "viralog:" line of
    Nothing -> Left line -- Not a viralog line, return original for raw printing
    Just jsonStr ->
      case decode (encodeUtf8 jsonStr) of
        Nothing -> Left line -- Invalid JSON, return original for raw printing
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

Renders viralog entries with emoji and colored text.
Displays context key-value pairs if present.
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
     in span_ [class_ textClass] $ do
          toHtml emoji
          toHtml (" " :: Text)
          toHtml viraLog.message
          unless (null ctx) $ do
            toHtml (" " :: Text)
            span_ [class_ "text-slate-500 dark:text-slate-600"] $ do
              toHtml ("{" :: Text)
              forM_ (intersperse Nothing $ map Just ctx) $ \case
                Nothing -> toHtml (", " :: Text)
                Just (k, v) -> toHtml $ k <> "=" <> v
              toHtml ("}" :: Text)
          br_ []
  toHtmlRaw = toHtml
