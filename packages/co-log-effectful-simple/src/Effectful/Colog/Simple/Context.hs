{-# OPTIONS_GHC -Wno-orphans #-}

{- | Logging context for accumulating key-value metadata.

>>> import Effectful.Colog.Simple.Context
-}
module Effectful.Colog.Simple.Context (
  -- * Context
  LogContext (..),
  withLogContext,
) where

import Colog.Message (FieldType, RichMessage)
import Data.Time (UTCTime)
import Data.Time.LocalTime (TimeZone)
import Effectful (Eff, (:>))
import Effectful.Colog (Log)
import Effectful.Reader.Static qualified as ER
import Text.Show qualified

-- | Type alias for logging context stored in Reader
newtype LogContext = LogContext [(Text, Text)]
  deriving newtype (Semigroup, Monoid, Eq)

-- | Custom field type for storing logging context (key-value pairs)
type instance FieldType "context" = LogContext

-- | Field type for UTC timestamp
type instance FieldType "utcTime" = UTCTime

-- | Field type for timezone
type instance FieldType "timezone" = TimeZone

instance Text.Show.Show LogContext where
  show (LogContext ctx) =
    if null ctx
      then ""
      else
        "\ESC[37m  {" <> intercalate ", " (fmap showKeyValue ctx) <> "}\ESC[0m"
    where
      showKeyValue (k, v) = toString $ k <> "=" <> v

{- | Add context field to all log messages within the given action.

Context accumulates: nested calls will merge their contexts together.

>>> withLogContext [("taskId", "42")] $ do
>>>   log Info "Starting"  -- Will have {taskId=42} in props
>>>   withLogContext [("step", "compile")] $ do
>>>     log Info "Compiling"  -- Will have {taskId=42, step=compile}
-}
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
