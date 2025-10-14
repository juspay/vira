{-# LANGUAGE RecordWildCards #-}

{- | Logging utilities for git-effectful

>>> import Effectful.Git.Logging
-}
module Effectful.Git.Logging (
  log,
) where

import Colog.Core (Severity (..))
import Colog.Message (Msg (..), RichMessage, RichMsg (..), defaultFieldMap)
import Effectful (Eff, (:>))
import Effectful.Colog (Log, logMsg)

{- | Log a message with the given severity.

Ref: https://github.com/eldritch-cookie/co-log-effectful/issues/1

>>> import Effectful.Git.Logging (log, Severity(Info))
>>> log Info "Hello, world!"
-}
log :: forall es. (HasCallStack, Log (RichMessage IO) :> es) => Severity -> Text -> Eff es ()
log msgSeverity msgText =
  withFrozenCallStack $ logMsg $ RichMsg {richMsgMsg = Msg {msgStack = callStack, ..}, richMsgMap = defaultFieldMap}
