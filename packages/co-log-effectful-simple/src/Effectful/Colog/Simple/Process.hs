{-# LANGUAGE LambdaCase #-}

{- | Process logging utilities for co-log-effectful-simple.

This module provides utilities for logging process commands.
-}
module Effectful.Colog.Simple.Process (
  logCommand,
  formatCommandForLog,
) where

import Colog.Core (Severity)
import Colog.Message (RichMessage)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Reader.Static qualified as ER
import System.Console.ANSI (SGR (..), setSGRCode)
import System.Console.ANSI.Types (Color (..), ColorIntensity (..), ConsoleLayer (..))
import System.Process (CmdSpec (..), CreateProcess (..))

{- | Log a command that is about to be executed.

Formats the command with a shell prompt prefix for visual distinction.

>>> logCommand Info cloneCmd
-}
logCommand :: forall es. (HasCallStack, ER.Reader LogContext :> es, Log (RichMessage IO) :> es, IOE :> es) => Severity -> CreateProcess -> Eff es ()
logCommand severity cmd =
  withFrozenCallStack $ log severity $ formatCommandForLog cmd

{- | Format a CreateProcess command for logging with grey colour and $ prefix.

Returns a coloured shell command string suitable for log output.
-}
formatCommandForLog :: CreateProcess -> Text
formatCommandForLog cmd =
  toText (setSGRCode [SetColor Foreground Dull White]) <> "$ " <> formatCommand (cmdspec cmd) <> toText (setSGRCode [Reset])
  where
    formatCommand :: CmdSpec -> Text
    formatCommand = \case
      ShellCommand shellCmd -> toText shellCmd
      RawCommand prog args ->
        let quotedArgs = map quoteArg args
         in unwords (toText prog : quotedArgs)

    -- Quote argument if it contains spaces or special characters
    quoteArg :: String -> Text
    quoteArg arg =
      let argText = toText arg
       in if T.any (\c -> c `elem` [' ', '\t', '\n', '\'', '"', '\\']) argText
            then "'" <> T.replace "'" "'\\''" argText <> "'"
            else argText
