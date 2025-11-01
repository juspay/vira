{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Simple context-aware logging for @co-log@ with @effectful@.

>>> import Effectful.Colog.Simple
-}
module Effectful.Colog.Simple (
  -- * Logging
  log,

  -- * Context
  module Effectful.Colog.Simple.Context,

  -- * Thread utilities
  tagCurrentThread,

  -- * Severity
  Severity (..),
  severityEmoji,

  -- * Runner
  runLogActionStdout,
)
where

import Colog.Core (Severity (..))
import Colog.Message (FieldType, MessageField (..), Msg (..), RichMessage, RichMsg (..), defaultFieldMap, extractField)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Time (UTCTime, utcToLocalTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.Format qualified
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log, LogAction (LogAction), logMsg, runLogAction)
import Effectful.Colog.Simple.Context
import Effectful.Colog.Simple.Thread (tagCurrentThread)
import Effectful.Colog.Simple.Thread qualified as Thread
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Reader.Static qualified as ER
import GHC.TypeLits (KnownSymbol, Symbol)
import Type.Reflection (TypeRep, typeRep)

{- | Log a message with the given 'Severity'.

Ref: https://github.com/eldritch-cookie/co-log-effectful/issues/1

>>> import Effectful.Colog.Simple (log, Severity(Info))
>>> log Info "Hello, world!"
-}
log :: forall es. (HasCallStack, ER.Reader LogContext :> es, Log (RichMessage IO) :> es, IOE :> es) => Severity -> Text -> Eff es ()
log msgSeverity msgText = do
  -- Get context from Reader
  ctx <- ER.ask @LogContext
  let fieldMap = buildFieldMap ctx
      msg = Msg {msgStack = callStack, msgSeverity = msgSeverity, msgText = msgText}
  withFrozenCallStack $ logMsg $ RichMsg {richMsgMsg = msg, richMsgMap = fieldMap}

-- | Build field map with 'LogContext', timestamp and timezone
buildFieldMap :: LogContext -> DMap.DMap TypeRep (MessageField IO)
buildFieldMap ctx =
  defaultFieldMap
    <> DMap.fromList
      [ typeRep @"context" :=> MessageField (pure ctx)
      , typeRep @"timezone" :=> MessageField (liftIO getCurrentTimeZone)
      ]

-- | Get emoji for 'Severity' level
severityEmoji :: Severity -> Text
severityEmoji = \case
  Debug -> "🐛"
  Info -> "ℹ️"
  Warning -> "⚠️"
  Error -> "❌"

-- | Like 'Effectful.Colog.runLogAction' but works with 'RichMessage', writes to stdout, and filters by 'Severity'
runLogActionStdout :: Severity -> Eff '[ER.Reader LogContext, Log (RichMessage IO), IOE] a -> Eff '[IOE] a
runLogActionStdout minSeverity action =
  runLogAction logAction (ER.runReader mempty action)
  where
    logAction = LogAction $ \msg -> do
      when (msgSeverity msg.richMsgMsg >= minSeverity) $ do
        formatted <- unsafeEff_ $ fmtRichMessage msg
        putTextLn formatted

-- | Custom rich message formatter that includes timestamp, severity with colors, and call stack info
fmtRichMessage :: RichMessage IO -> IO Text
fmtRichMessage msg = do
  timeStr <- formatTime <$> getField @"utcTime" <*> getField @"timezone"

  threadIdText <-
    getField @"threadId" >>= \case
      Just tid -> Thread.threadDesc tid
      Nothing -> pure Thread.defaultThreadLabel

  mContext :: Maybe LogContext <- getField @"context"

  let message =
        timeStr
          <> " "
          <> formatSeverity msg.richMsgMsg.msgSeverity
          <> " ["
          <> threadIdText
          <> "] "
          <> msg.richMsgMsg.msgText
          <> maybe mempty show mContext
  pure $ case msg.richMsgMsg.msgSeverity of
    Debug -> "\ESC[90m" <> message <> "\ESC[0m"
    Info -> message
    Warning -> "\ESC[33m" <> message <> "\ESC[0m"
    Error -> "\ESC[31m" <> message <> "\ESC[0m"
  where
    getField :: forall (fieldName :: Symbol). (KnownSymbol fieldName) => IO (Maybe (FieldType fieldName))
    getField = extractField (DMap.lookup (typeRep @fieldName) msg.richMsgMap)

    formatSeverity :: Severity -> Text
    formatSeverity sev = case sev of
      Debug -> severityEmoji sev <> " DEBUG"
      Info -> severityEmoji sev <> "  INFO"
      Warning -> severityEmoji sev <> "  WARN"
      Error -> severityEmoji sev <> " ERROR"

    formatTime :: Maybe UTCTime -> Maybe TimeZone -> Text
    formatTime mUtcTime mTimeZone = case (mUtcTime, mTimeZone) of
      (Just t, Just tz) ->
        let localTime = utcToLocalTime tz t
         in toText $ Data.Time.Format.formatTime defaultTimeLocale "%H:%M" localTime
      _ -> "??:??"
