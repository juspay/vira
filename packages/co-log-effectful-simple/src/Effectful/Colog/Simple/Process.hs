{-# LANGUAGE LambdaCase #-}

{- | Process logging utilities for co-log-effectful-simple.

This module provides utilities for logging process commands.
-}
module Effectful.Colog.Simple.Process (
  logCommand,
) where

import Colog.Core (Severity)
import Colog.Message (RichMessage)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Reader.Static qualified as ER
import System.Process (CmdSpec (..), CreateProcess (..))

{- | Log a command that is about to be executed.

Formats the command as a shell command string for readability.

>>> logCommand Info "Running git clone" cloneCmd
-}
logCommand :: forall es. (HasCallStack, ER.Reader LogContext :> es, Log (RichMessage IO) :> es, IOE :> es) => Severity -> Text -> CreateProcess -> Eff es ()
logCommand severity prefix cmd =
  withFrozenCallStack $ log severity $ prefix <> ": " <> formatCommand (cmdspec cmd)
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
