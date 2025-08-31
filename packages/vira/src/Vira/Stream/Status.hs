{-# LANGUAGE OverloadedRecordDot #-}

-- | Real-time status of the Vira system.
module Vira.Stream.Status (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStream,
  indicator,
) where

import Control.Concurrent (threadDelay)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Effectful (Eff)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseConnect_, hxSseSwap_)
import Servant.API
import Servant.API.EventStream
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Lucid (AppHtml, getLinkUrl)
import Vira.App.Stack (AppStack)
import Vira.State.Acid qualified as Acid
import Vira.State.Type
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO Status))

-- A status message sent from server to client
--
-- The `Int` is the unique identifier of the status message, which contains the
-- raw HTML of the status.
data Status
  = Status Int (Html ())
  | Refresh Text
  | CurrentTime (Html ())

instance ToServerEvent Status where
  toServerEvent = \case
    Status ident t ->
      ServerEvent
        (Just "status")
        (Just $ show ident)
        (Lucid.renderBS t)
    Refresh js ->
      ServerEvent
        (Just "refresh")
        Nothing
        (encodeUtf8 js)
    CurrentTime t ->
      ServerEvent
        (Just "currenttime")
        Nothing
        (Lucid.renderBS t)

viewStream :: AppHtml ()
viewStream = do
  link <- lift $ getLinkUrl LinkTo.StatusGet
  div_ [hxExt_ "sse", hxSseConnect_ link] $ do
    div_ [class_ "fixed top-4 right-4 flex items-center space-x-4"] $ do
      div_ [hxSseSwap_ "status"] viewInner
      div_ [hxSseSwap_ "currenttime", class_ "text-xs text-gray-500 px-1.5 py-0.5 rounded font-mono"] viewCurrentTime
    script_ [hxSseSwap_ "refresh"] ("" :: Text)

-- | Status view for both immediate display and SSE streaming
viewInner :: AppHtml ()
viewInner = do
  -- Compute running jobs directly
  jobsData <- lift $ App.query Acid.GetRunningJobs
  let jobs = jobsData <&> \job -> (job.jobRepo, job.jobId)
  div_ [class_ "flex items-center space-x-2", title_ "Build Status"] $ do
    indicator $ not $ null jobs
    forM_ jobs $ \(repo, jobId) -> do
      jobUrl <- lift $ getLinkUrl $ LinkTo.Job jobId
      a_ [href_ jobUrl] $ do
        span_ $ b_ $ toHtml $ unRepoName repo
        "/"
        span_ $ code_ $ toHtml @Text $ show jobId

-- | Current time view for both immediate display and SSE streaming
viewCurrentTime :: AppHtml ()
viewCurrentTime = do
  utcTime <- liftIO getCurrentTime
  timeZone <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime timeZone utcTime
  let timeStr = formatTime defaultTimeLocale "%H:%M:%S" localTime
  toHtml timeStr

indicator :: (Monad m) => Bool -> HtmlT m ()
indicator active = do
  let (iconSvg, classes) =
        if active
          then (Icon.loader_2, "text-green-500 animate-spin")
          else (Icon.circle, "text-gray-500")
  div_ [class_ $ "w-4 h-4 flex items-center justify-center " <> classes] $
    toHtmlRaw iconSvg

streamRouteHandler :: SourceT (Eff AppStack) Status
streamRouteHandler = S.fromStepT $ step 0
  where
    step (n :: Int) = S.Effect $ do
      when (n > 0) $ do
        liftIO $ threadDelay 1_000_000

      -- Always send current time every second
      timeHtml <- App.runAppHtmlHandlingError viewCurrentTime
      let timeMsg = CurrentTime timeHtml

      -- Also send refresh every 5 seconds
      if n `mod` 5 == 0 && n > 0
        then do
          let refreshMsg = Refresh "location.reload()"
          pure $ S.Yield timeMsg $ S.Effect $ do
            pure $ S.Yield refreshMsg $ step (n + 1)
        else
          pure $ S.Yield timeMsg $ step (n + 1)
