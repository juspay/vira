{-# LANGUAGE RecordWildCards #-}

{- | To use logging, import this module unqualified.

>>> import Vira.Lib.Logging
-}
module Vira.Lib.Logging (
  -- * Logging
  log,
  runLogActionStdout,
)
where

import Colog.Core (Severity (..))
import Colog.Message (Message, Msg (..), fmtMessage)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log, LogAction (LogAction), logMsg, runLogAction)

-- | Like `runLogAction` but works with `Message` and writes to `Stdout` (the common use-case)
runLogActionStdout :: Eff '[Log Message, IOE] a -> Eff '[IOE] a
runLogActionStdout =
  runLogAction logAction
  where
    logAction = LogAction $ \m ->
      putTextLn $ fmtMessage m

{- | Log a message with the given severity.

Ref: https://github.com/eldritch-cookie/co-log-effectful/issues/1

>>> import Vira.Lib.Logging (log, Severity(Info))
>>> log Info "Hello, world!"
-}
log :: forall es. (HasCallStack, Log Message :> es) => Severity -> Text -> Eff es ()
log msgSeverity msgText =
  withFrozenCallStack $ logMsg $ Msg {msgStack = callStack, ..}
