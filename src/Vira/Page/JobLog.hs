{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infinitely" #-}

module Vira.Page.JobLog where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TQueue, flushTQueue, newTQueueIO, writeTQueue)
import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Htmx.Lucid.Core (hxSwap_)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Servant hiding (throwError)
import Servant.API.EventStream (ServerEvent (ServerEvent), ServerSentEvents, ToServerEvent (toServerEvent))
import Servant.Server.Generic (AsServer)
import Servant.Types.SourceT qualified as S
import System.FilePath ((</>))
import System.IO (hGetLine)
import System.Process (CreateProcess (std_out), ProcessHandle, StdStream (CreatePipe), createProcess, proc, terminateProcess)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.HTMX (hxSseConnect_, hxSseSwap_)
import Vira.State.Acid qualified as St
import Vira.State.Type (Job, JobId, jobWorkingDir)
import Vira.State.Type qualified as St
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { -- Raw build log
    _rawLog :: mode :- "raw" :> Get '[PlainText] Text
  , -- Stream build log
    _streamLog :: mode :- "stream" :> ServerSentEvents (SourceIO LogChunk)
  }
  deriving stock (Generic)

handlers :: App.AppState -> JobId -> Routes AsServer
handlers cfg jobId = do
  Routes
    { _rawLog = App.runAppInServant cfg $ rawLogHandler jobId
    , _streamLog = pure $ streamHandler cfg jobId
    }

rawLogHandler :: JobId -> Eff App.AppServantStack Text
rawLogHandler jobId = do
  job <- App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  logText <-
    liftIO $ readFileBS $ job.jobWorkingDir </> "output.log"
  pure $ decodeUtf8 logText

data LogChunk = LogChunk Int (Maybe TailF) (Html ())

instance ToServerEvent LogChunk where
  toServerEvent (LogChunk ident _handle t) =
    ServerEvent
      (Just "logchunk")
      (Just $ show ident)
      (Lucid.renderBS t)

-- Represent a `tail -f` process along with its output gathered up in TChat
data TailF = TailF Int FilePath (TQueue Text)

newTailF :: Int -> FilePath -> IO TailF
newTailF n filePath = do
  TailF n filePath <$> newTQueueIO

runTailF :: TailF -> IO ProcessHandle
runTailF (TailF n filePath chan) = do
  -- Run `tail -f` using System.Process and stream its output to chan
  (_, Just hOut, _, ph) <- createProcess (proc "tail" ["-n", show n, "-f", filePath]) {std_out = CreatePipe}
  hSetBuffering hOut LineBuffering
  void $ forkIO $ do
    let loop = do
          hIsEOF hOut >>= \case
            True -> pass
            False -> do
              line <- hGetLine hOut
              atomically $ writeTQueue chan $ toText line
              loop
    loop
  -- Give the process a chance to start up
  threadDelay 100_000
  pure ph

tryReadTailF :: TailF -> IO (Maybe Text)
tryReadTailF (TailF _ _ chan) = do
  ls <- atomically $ flushTQueue chan
  if null ls
    then pure Nothing
    else pure $ Just $ unlines ls

streamHandler :: App.AppState -> JobId -> S.SourceT IO LogChunk
streamHandler cfg jobId = S.fromStepT $ step 0 Nothing
  where
    step (n :: Int) (mh :: Maybe (TailF, ProcessHandle)) = S.Effect $ do
      job :: Job <-
        App.runApp cfg $
          App.query (St.GetJobA jobId)
            >>= maybe (error "Job not found") pure
      let jobActive = job.jobStatus == St.JobRunning || job.jobStatus == St.JobPending
      let logFile = job.jobWorkingDir </> "output.log"
      (h, p) <-
        maybe
          ( liftIO $ do
              t <- newTailF 1000 logFile
              p <- runTailF t
              pure (t, p)
          )
          pure
          mh
      -- TODO:
      -- 1. Read last N lines, rather than reading from scratch
      -- 2. Send chunks, not single line. And delay?
      mLine <- tryReadTailF h
      case (mLine, jobActive) of
        (Nothing, True) -> do
          threadDelay 100_000
          pure $ S.Skip $ step (n + 1) (Just (h, p))
        (Nothing, False) -> do
          -- TODO: Drain all of tail -f's output first
          terminateProcess p
          -- FIXME:
          -- pure S.Stop
          putStrLn "!!!!!! Due to sse bug, waiting infinitely"
          forever $ do
            threadDelay 1_000_000_000
            pure S.Stop
        (Just line, _) -> do
          let msg = LogChunk n (Just h) (pre_ $ toHtml line)
          pure $ S.Yield msg $ step (n + 1) (Just (h, p))

viewStream :: (LinkTo.LinkTo -> Link) -> St.Job -> Html ()
viewStream linkTo job = do
  div_ $ do
    div_ [class_ "my-2"] $ do
      p_ $ do
        a_
          [target_ "blank", class_ "underline text-blue-500", href_ $ show . linkURI $ linkTo $ LinkTo.JobLog job.jobId]
          "View Full Log"
    let streamLink = show . linkURI $ linkTo $ LinkTo.JobLogStream job.jobId
    let sseAttrs =
          [ hxExt_ "sse"
          , hxSseConnect_ streamLink
          , hxSwap_ "beforeend"
          , hxSseSwap_ "logchunk"
          ]
    pre_ (sseAttrs <> [class_ "bg-black text-white p-2 text-xs", style_ "white-space: pre-wrap;"]) $ code_ $ do
      "Loading log ..."
    div_ "TODO: add running or finished indicator"
