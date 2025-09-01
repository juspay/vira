{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

{- | To use logging, import this module unqualified.

>>> import Vira.Lib.Logging
-}
module Vira.Lib.Logging (
  -- * Logging
  log,
  runLogActionStdout,
  tagCurrentThread,
)
where

import Colog.Core (Severity (..))
import Colog.Message (Message, Msg (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log, LogAction (LogAction), logMsg, runLogAction)
import Effectful.Dispatch.Static (unsafeEff_)
import GHC.Conc (ThreadId, labelThread, myThreadId)
import GHC.Conc.Sync (threadLabel)
import GHC.Stack (SrcLoc (srcLocModule))

tagCurrentThread :: (MonadIO m) => String -> m ()
tagCurrentThread tag = do
  tid <- liftIO myThreadId
  liftIO $ labelThread tid tag

threadIdToInt :: ThreadId -> Text
threadIdToInt tid =
  let s = show tid
   in fromMaybe s $ T.stripPrefix "ThreadId " s

-- | Custom rich message formatter that includes timestamp, severity with colors, and call stack info
fmtRichMessage :: Message -> IO Text
fmtRichMessage Msg {..} = do
  threadId <- myThreadId
  threadIdText <-
    threadLabel threadId >>= \case
      Nothing -> pure $ threadIdToInt threadId
      Just label -> pure $ toText label <> ":" <> threadIdToInt threadId
  let severityText = case msgSeverity of
        Debug -> "🐛 DEBUG"
        Info -> "ℹ️  INFO "
        Warning -> "⚠️  WARN "
        Error -> "❌ ERROR"
      props =
        Map.fromList $
          catMaybes
            [ ("mod",) <$> getNonLogCaller msgStack
            ]
      message = severityText <> " [" <> threadIdText <> "] " <> msgText <> showProps props
  pure $ case msgSeverity of
    Debug -> "\ESC[90m" <> message <> "\ESC[0m"
    Info -> message
    Warning -> "\ESC[33m" <> message <> "\ESC[0m"
    Error -> "\ESC[31m" <> message <> "\ESC[0m"

showProps :: Map Text Text -> Text
showProps prop =
  "\ESC[37m  {" <> T.intercalate ", " (map showKeyValue (Map.toAscList prop)) <> "}\ESC[0m"
  where
    showKeyValue (k, v) = k <> "=" <> v

getNonLogCaller :: CallStack -> Maybe Text
getNonLogCaller stack =
  go $ getCallStack stack
  where
    go = \case
      [] -> Nothing
      ("log", _) : rest -> go rest -- HACK: Ignore the `log` function below
      ("logWithStreamId", _) : rest -> go rest -- HACK: Ignore the `log` function below
      (funcName, loc) : _ -> Just $ toText $ loc.srcLocModule <> ":" <> funcName

-- | Like `runLogAction` but works with `Message` and writes to `Stdout` (the common use-case)
runLogActionStdout :: Eff '[Log Message, IOE] a -> Eff '[IOE] a
runLogActionStdout =
  runLogAction logAction
  where
    logAction = LogAction $ \m -> do
      formatted <- unsafeEff_ $ fmtRichMessage m
      putTextLn formatted

{- | Log a message with the given severity.

Ref: https://github.com/eldritch-cookie/co-log-effectful/issues/1

>>> import Vira.Lib.Logging (log, Severity(Info))
>>> log Info "Hello, world!"
-}
log :: forall es. (HasCallStack, Log Message :> es) => Severity -> Text -> Eff es ()
log msgSeverity msgText =
  withFrozenCallStack $ logMsg $ Msg {msgStack = callStack, ..}
