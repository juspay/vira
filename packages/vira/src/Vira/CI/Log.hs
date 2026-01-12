{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Structured logging for Vira pipeline messages

'ViraLog' messages are written as JSON to @output.log@.
Web UI renders them with 'Colog.Severity'-specific styling (emoji + colored background).
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
import LogSink.Contrib.NixNoise (NixNoise (..), decodeNixNoise)
import Lucid (HtmlT, ToHtml (..), class_, details_, div_, span_, summary_, toHtml)
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

-- | Encode ViraLog to JSON
encodeViraLog :: ViraLog -> Text
encodeViraLog viraLog = decodeUtf8 (encode viraLog)

{- | Decode ViraLog from a JSON line
Returns Left with original line if not valid JSON (for raw printing)
-}
decodeViraLog :: Text -> Either Text ViraLog
decodeViraLog line
  | not ("{" `T.isPrefixOf` line) = Left line -- Fast fail for non-JSON
  | otherwise = case decode (encodeUtf8 line) of
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

Renders viralog entries with emoji and colored text.
Displays context key-value pairs if present.
-}
instance ToHtml ViraLog where
  toHtml viraLog =
    -- The message field may contain NixNoise JSON when this is a noise entry
    case decodeNixNoise viraLog.message of
      Just noise -> renderNoiseBlock noise.noiseLines
      Nothing -> renderRegularLog viraLog
    where
      renderNoiseBlock :: (Monad m) => Text -> HtmlT m ()
      renderNoiseBlock noiseText =
        let lineCount = length (lines noiseText)
         in details_ [class_ "opacity-60 hover:opacity-100 transition-opacity"] $ do
              summary_ [class_ "cursor-pointer text-slate-500 dark:text-slate-600 select-none"] $
                toHtml ("○ Nix input details (" <> show lineCount <> " lines)" :: Text)
              div_ [class_ "pl-4 text-sm opacity-70 whitespace-pre-wrap"] $
                toHtml noiseText
      renderRegularLog :: (Monad m) => ViraLog -> HtmlT m ()
      renderRegularLog vlog =
        let emoji = severityEmoji vlog.level
            textClass = case vlog.level of
              Debug -> "text-slate-400 dark:text-slate-500"
              Info -> "text-cyan-400 dark:text-cyan-500"
              Warning -> "text-amber-400 dark:text-amber-500"
              Error -> "text-rose-400 dark:text-rose-500"
            LogContext ctx = vlog.context
         in span_ [class_ textClass] $ do
              toHtml emoji
              toHtml (" " :: Text)
              toHtml vlog.message
              unless (null ctx) $ do
                toHtml (" " :: Text)
                span_ [class_ "text-slate-500 dark:text-slate-600"] $ do
                  toHtml ("{" :: Text)
                  forM_ (intersperse Nothing $ map Just ctx) $ \case
                    Nothing -> toHtml (", " :: Text)
                    Just (k, v) -> toHtml $ k <> "=" <> v
                  toHtml ("}" :: Text)
  toHtmlRaw = toHtml
