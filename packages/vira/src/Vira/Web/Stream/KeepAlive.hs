{- |
Generic wrapper for SSE streams with keep-alive heartbeats.

= Problem

Long-running operations (e.g., builds taking 5+ minutes) cause SSE connections
to timeout due to inactivity. Warp's default timeout is 30 seconds. When a
connection times out:

1. Browser auto-reconnects
2. @subscribeToBroadcasts@ drains pending messages (to avoid replaying old events)
3. Any broadcasts that arrived during disconnection are LOST ❌

= Solution

Wrap SSE event types with @KeepAlive a@ to send heartbeats every 15 seconds:

- @Data a@ - Actual SSE event (e.g., page reload signal)
- @Heartbeat@ - Keep-alive comment sent periodically

Combined with increased Warp timeout (600s), this ensures connections stay alive
indefinitely during long operations, preventing lost broadcasts.

= Usage

@
type MyStreamRoute = ServerSentEvents (SourceIO (KeepAlive MyEvent))

handler :: SourceT (Eff es) (KeepAlive MyEvent)
handler = ...
  timeout keepAliveInterval (consumeEvents chan) >>= \\case
    Nothing -> pure $ S.Yield Heartbeat step
    Just event -> pure $ S.Yield (Data event) step
@
-}
module Vira.Web.Stream.KeepAlive (
  KeepAlive (..),
  keepAliveInterval,
  withKeepAlive,
  yieldEvent,
) where

import Colog.Core (Severity (Debug))
import Colog.Message (RichMessage)
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Reader.Static qualified as ER
import Servant.API.EventStream (ServerEvent (..), ToServerEvent (..))
import Servant.Types.SourceT qualified as S
import UnliftIO.Timeout (timeout)

{- | Wrapper for SSE events that adds heartbeat support

Generic over the actual event type @a@, allowing reuse across different streams.
-}
data KeepAlive a
  = -- | Actual data event to send to client
    Data a
  | -- | Keep-alive comment (ignored by client, prevents timeout)
    Heartbeat
  deriving stock (Show, Eq)

instance (ToServerEvent a) => ToServerEvent (KeepAlive a) where
  toServerEvent = \case
    Data event -> toServerEvent event
    Heartbeat ->
      ServerEvent
        Nothing -- No event type (comment, ignored by client)
        Nothing -- Event ID
        ": keep-alive" -- SSE comment format

{- | Heartbeat interval to prevent connection timeout

Set to 15s << Warp timeout of 600s for safety margin.
Use with @timeout keepAliveInterval@ to detect when no events arrive.
-}
keepAliveInterval :: Int
keepAliveInterval = 15_000_000 -- 15 seconds in microseconds

{- | SSE stream step with automatic keep-alive handling

Encapsulates the entire keep-alive pattern including recursion:
1. Consumes from source with timeout
2. On timeout: logs heartbeat, continues with same state
3. On data: processes data, yields event or skips, continues with updated state

This completely hides KeepAlive implementation details from stream handlers.

Example usage:
@
step (n, patterns, chan) = KeepAlive.withKeepAlive
  (consumeBroadcasts chan)
  (\\scopes ->
    let matching = filter (matchesAnyScope patterns) (toList scopes)
    in if null matching
       then pure Nothing  -- Skip
       else pure $ Just (ScopedRefresh, (n+1, patterns, chan)))  -- Yield + new state
  (n, patterns, chan)
  step
@
-}
withKeepAlive ::
  ( IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  Eff es a ->
  (a -> Eff es (Maybe (b, state))) ->
  state ->
  (state -> S.StepT (Eff es) (KeepAlive b)) ->
  S.StepT (Eff es) (KeepAlive b)
withKeepAlive consumeAction processData currentState continue = S.Effect $ do
  timeout keepAliveInterval consumeAction >>= \case
    Nothing -> do
      log Debug "Sending heartbeat (no new data)"
      pure $ S.Yield Heartbeat $ continue currentState
    Just result -> do
      processData result >>= \case
        Nothing -> pure $ S.Skip $ continue currentState
        Just (event, newState) -> pure $ S.Yield (Data event) $ continue newState

{- | Yield an event without timeout/heartbeat logic

Use for cases where you need to yield an event outside the normal
consume-with-timeout pattern (e.g., stream termination events).
-}
yieldEvent :: a -> S.StepT m (KeepAlive a) -> S.StepT m (KeepAlive a)
yieldEvent event = S.Yield (Data event)
