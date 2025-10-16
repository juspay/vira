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

-- | Vira structured log entry
data ViraLog = ViraLog
  { level :: Severity
  , message :: Text
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
-}
renderViraLogCLI :: ViraLog -> Text
renderViraLogCLI vlog =
  let (emoji :: Text, intensity, clr) = case vlog.level of
        Debug -> ("🐛", Dull, White)
        Info -> ("ℹ️", Vivid, Cyan)
        Warning -> ("⚠️", Vivid, Yellow)
        Error -> ("❌", Vivid, Red)
      colorCode = setSGRCode [SetColor Foreground intensity clr]
      resetCode = setSGRCode [Reset]
   in toText colorCode <> emoji <> "  " <> vlog.message <> toText resetCode
