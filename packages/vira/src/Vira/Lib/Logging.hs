{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | To use logging, import this module unqualified.

>>> import Vira.Lib.Logging
-}
module Vira.Lib.Logging (
  -- * Logging
  log,
  withLogContext,
  tagCurrentThread,
  Severity (..), -- Add this
  LogContext,

  -- * Runner
  runLogActionStdout,
)
where

import Colog.Core (Severity (..))
import Colog.Message (FieldType, MessageField (..), Msg (..), RichMessage, RichMsg (..), defaultFieldMap, extractField)
import Data.Dependent.Map qualified as DMap
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log, LogAction (LogAction), logMsg, runLogAction)
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Reader.Static qualified as ER
import GHC.Conc (ThreadId, labelThread, myThreadId)
import GHC.Conc.Sync (threadLabel)
import Text.Show qualified as Show
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
log :: forall es. (HasCallStack, ER.Reader LogContext :> es, Log (RichMessage IO) :> es) => Severity -> Text -> Eff es ()
log msgSeverity msgText = do
  -- Get context from Reader
  ctx <- ER.ask @LogContext
  -- Create field map with context
  let contextField = MessageField (pure ctx) :: MessageField IO "context"
      fieldMap = DMap.insert (typeRep @"context") contextField defaultFieldMap
  withFrozenCallStack $ logMsg $ RichMsg {richMsgMsg = Msg {msgStack = callStack, ..}, richMsgMap = fieldMap}

{- | Add context field to all log messages within the given action.

Context accumulates: nested calls will merge their contexts together.

>>> withLogContext [("taskId", "42")] $ do
>>>   log Info "Starting"  -- Will have {taskId=42} in props
>>>   withLogContext [("step", "compile")] $ do
>>>     log Info "Compiling"  -- Will have {taskId=42, step=compile}
-}

-- | Type alias for logging context stored in Reader
newtype LogContext = LogContext [(Text, Text)]
  deriving newtype (Semigroup, Monoid, Eq)

-- | Custom field type for storing logging context (key-value pairs)
type instance FieldType "context" = LogContext

instance Show LogContext where
  show (LogContext ctx) =
    if null ctx
      then ""
      else
        "\ESC[37m  {" <> intercalate ", " (showKeyValue <$> ctx) <> "}\ESC[0m"
    where
      showKeyValue (k, v) = toString $ k <> "=" <> v

withLogContext ::
  forall es a.
  ( ER.Reader LogContext :> es
  , Log (RichMessage IO) :> es
  ) =>
  [(Text, Text)] ->
  Eff es a ->
  Eff es a
withLogContext pairs action = do
  -- Modify the context in the Reader effect (append to preserve order)
  ER.local (<> LogContext pairs) action

-- | Like `runLogAction` but works with `RichMessage`, writes to `Stdout`, and filters by severity
runLogActionStdout :: Severity -> Eff '[ER.Reader LogContext, Log (RichMessage IO), IOE] a -> Eff '[IOE] a
runLogActionStdout minSeverity action =
  runLogAction logAction (ER.runReader mempty action)
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
    Nothing -> pure defaultThreadLabel

  -- Extract context from the field map
  mContext :: Maybe LogContext <- extractField $ DMap.lookup (typeRep @"context") richMsgMap

  let severityText = case msgSeverity of
        Debug -> "🐛 DEBUG"
        Info -> "ℹ️  INFO "
        Warning -> "⚠️  WARN "
        Error -> "❌ ERROR"
      message = severityText <> " [" <> threadIdText <> "] " <> msgText <> maybe mempty show mContext
  pure $ case msgSeverity of
    Debug -> "\ESC[90m" <> message <> "\ESC[0m"
    Info -> message
    Warning -> "\ESC[33m" <> message <> "\ESC[0m"
    Error -> "\ESC[31m" <> message <> "\ESC[0m"

{- | A short descriptive identifier for ThreadId

Includes thread label (if any) as well as the ID.
-}
threadDesc :: ThreadId -> IO Text
threadDesc tid = do
  label <- threadLabel tid <&> maybe defaultThreadLabel toText
  pure $ toText label <> ";" <> threadIdToInt
  where
    -- Unfortunately there is no way to get the integer out of `ThreadId`, so must HACK around it.
    threadIdToInt :: Text
    threadIdToInt =
      let s = show tid
       in fromMaybe s $ T.stripPrefix "ThreadId " s

defaultThreadLabel :: Text
defaultThreadLabel = "🧵"
