{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

{- | To use logging, import this module unqualified.

>>> import Vira.Lib.Logging
-}
module Vira.Lib.Logging (
  -- * Logging
  log,
  tagCurrentThread,
  Severity (..), -- Add this

  -- * Runner
  runLogActionStdout,
)
where

import Colog.Core (Severity (..))
import Colog.Message (Msg (..), RichMessage, RichMsg (..), defaultFieldMap, extractField)
import Data.Dependent.Map qualified as DMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log, LogAction (LogAction), logMsg, runLogAction)
import Effectful.Dispatch.Static (unsafeEff_)
import GHC.Conc (ThreadId, labelThread, myThreadId)
import GHC.Conc.Sync (threadLabel)
import GHC.Stack (SrcLoc (srcLocModule))
import Type.Reflection (typeRep)

-- | Add a label to the current thread
tagCurrentThread :: (MonadIO m) => String -> m ()
tagCurrentThread tag = do
  tid <- liftIO myThreadId
  liftIO $ labelThread tid tag

{- | Log a message with the given severity.

Ref: https://github.com/eldritch-cookie/co-log-effectful/issues/1

>>> import Vira.Lib.Logging (log, Severity(Info))
>>> log Info "Hello, world!"
-}
log :: forall es. (HasCallStack, Log (RichMessage IO) :> es) => Severity -> Text -> Eff es ()
log msgSeverity msgText =
  withFrozenCallStack $ logMsg $ RichMsg {richMsgMsg = Msg {msgStack = callStack, ..}, richMsgMap = defaultFieldMap}

-- | Like `runLogAction` but works with `RichMessage`, writes to `Stdout`, and filters by severity
runLogActionStdout :: Severity -> Eff '[Log (RichMessage IO), IOE] a -> Eff '[IOE] a
runLogActionStdout minSeverity =
  runLogAction logAction
  where
    logAction = LogAction $ \richMsg -> do
      when (msgSeverity richMsg.richMsgMsg >= minSeverity) $ do
        formatted <- unsafeEff_ $ fmtRichMessage richMsg
        putTextLn formatted

-- | Custom rich message formatter that includes timestamp, severity with colors, and call stack info
fmtRichMessage :: RichMessage IO -> IO Text
fmtRichMessage RichMsg {..} = do
  let Msg {..} = richMsgMsg
  -- Extract ThreadId from the field map
  mThreadId <- extractField $ DMap.lookup (typeRep @"threadId") richMsgMap
  threadIdText <- case mThreadId of
    Just tid -> threadDesc tid
    Nothing -> pure "🧵;?"

  let severityText = case msgSeverity of
        Debug -> "🐛 DEBUG"
        Info -> "ℹ️  INFO "
        Warning -> "⚠️  WARN "
        Error -> "❌ ERROR"
      props =
        Map.fromList $
          catMaybes
            [ ("mod",) <$> getNonLogCaller msgStack
            -- Note: richMsgMap contains threadId and utcTime fields via defaultFieldMap
            -- ThreadId is extracted above for display in the message
            -- Additional custom fields can be added via the field map in the future
            ]
      message = severityText <> " [" <> threadIdText <> "] " <> msgText <> showProps props
  pure $ case msgSeverity of
    Debug -> "\ESC[90m" <> message <> "\ESC[0m"
    Info -> message
    Warning -> "\ESC[33m" <> message <> "\ESC[0m"
    Error -> "\ESC[31m" <> message <> "\ESC[0m"

-- Unfortunately there is no way to get the integer out of `ThreadId`, so must HACK around it.
threadIdToInt :: ThreadId -> Text
threadIdToInt tid =
  let s = show tid
   in fromMaybe s $ T.stripPrefix "ThreadId " s

{- | A short descriptive identifier for ThreadId

Includes thread label (if any) as well as the ID.
-}
threadDesc :: ThreadId -> IO Text
threadDesc tid = do
  label <- threadLabel tid <&> maybe "🧵" toText
  pure $ toText label <> ";" <> threadIdToInt tid

showProps :: Map Text Text -> Text
showProps prop =
  if null prop
    then ""
    else
      "\ESC[37m  {" <> T.intercalate ", " (map showKeyValue (Map.toAscList prop)) <> "}\ESC[0m"
  where
    showKeyValue (k, v) = k <> "=" <> v

-- HACK: Show 'effective' caller, ignoring immediate logging functions.
getNonLogCaller :: CallStack -> Maybe Text
getNonLogCaller stack =
  go $ getCallStack stack
  where
    go = \case
      [] -> Nothing
      ("log", _) : rest -> go rest -- HACK: Ignore the `log` function below
      (funcName, loc) : _ -> Just $ toText $ loc.srcLocModule <> ":" <> funcName
