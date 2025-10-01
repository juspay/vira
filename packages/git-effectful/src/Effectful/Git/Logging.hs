{-# LANGUAGE RecordWildCards #-}

{- | Logging utilities for git-effectful

>>> import Effectful.Git.Logging
-}
module Effectful.Git.Logging (
  log,
) where

import Colog.Core (Severity (..))
import Colog.Message (Message, Msg (..))
import Effectful (Eff, (:>))
import Effectful.Colog (Log, logMsg)

{- | Log a message with the given severity.

Ref: https://github.com/eldritch-cookie/co-log-effectful/issues/1

>>> import Effectful.Git.Logging (log, Severity(Info))
>>> log Info "Hello, world!"
-}
log :: forall es. (HasCallStack, Log Message :> es) => Severity -> Text -> Eff es ()
log msgSeverity msgText =
  withFrozenCallStack $ logMsg $ Msg {msgStack = callStack, ..}
