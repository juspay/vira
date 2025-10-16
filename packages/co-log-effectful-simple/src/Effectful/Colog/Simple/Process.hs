{-# LANGUAGE LambdaCase #-}

{- | Process logging utilities for co-log-effectful-simple.

This module provides utilities for logging process commands.
-}
module Effectful.Colog.Simple.Process (
  withLogCommand,
  formatCommandForLog,
) where

import Colog.Message (RichMessage)
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, withLogContext)
import Effectful.Reader.Static qualified as ER
import System.Process (CmdSpec (..), CreateProcess (..))

{- | Execute an action with command logging context.

Adds the formatted command as context under the "cmd" key.
Caller is responsible for logging if desired.

>>> withLogCommand cloneCmd $ do
>>>   log Info "Running git clone"
>>>   readCreateProcess cloneCmd ""
-}
withLogCommand :: forall es a. (ER.Reader LogContext :> es, Log (RichMessage IO) :> es) => CreateProcess -> Eff es a -> Eff es a
withLogCommand cmd =
  withLogContext [("cmd", formatCommandForLog cmd)]

{- | Format a CreateProcess command for logging.

Returns a shell command string suitable for log output.
-}
formatCommandForLog :: CreateProcess -> Text
formatCommandForLog cmd = formatCommand (cmdspec cmd)
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
