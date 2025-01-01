{-# LANGUAGE RecordWildCards #-}

{- | To use logging, import this module unqualified.

>>> import Vira.App.Logging

You do not need to import any other module to use logging, since this one re-exports all the necessary types and functions.
-}
module Vira.App.Logging (
  -- * Logging
  log,

  -- * Re-exports from co-log
  Severity (..),
  Message,
  Log,

  -- * For logging in IO monads
  runViraLog,
)
where

import Colog.Core (Severity (..))
import Colog.Message (Message, Msg (..), fmtMessage)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Colog (Log, LogAction (LogAction), logMsg, runLogAction)

-- | Like `runLogActionStdout` but useful in `IO` monads.
runViraLog :: Eff '[Log Message, IOE] a -> IO a
runViraLog =
  runEff . runLogActionStdout

-- | Like `runLogAction` but works with `Message` and writes to `Stdout` (the common use-case)
runLogActionStdout :: Eff '[Log Message, IOE] a -> Eff '[IOE] a
runLogActionStdout =
  runLogAction logAction
  where
    logAction = LogAction $ \m ->
      putTextLn $ fmtMessage m

{- | Log a message with the given severity.

>>> import Vira.App.Logging (log, Severity(Info))
>>> log Info "Hello, world!"
-}
log :: forall es. (HasCallStack, Log Message :> es) => Severity -> Text -> Eff es ()
log msgSeverity msgText =
  withFrozenCallStack $ logMsg $ Msg {msgStack = callStack, ..}
