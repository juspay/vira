{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infinitely" #-}

module Vira.Page.JobLog where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, ioError)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID4
import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Lucid
import Servant hiding (throwError)
import Servant.API.EventStream (ServerEvent (ServerEvent), ServerSentEvents, ToServerEvent (toServerEvent))
import Servant.Server.Generic (AsServer)
import Servant.Types.SourceT qualified as S
import System.FilePath ((</>))
import System.IO (hGetLine)
import System.IO.Error (isEOFError)
import Vira.App qualified as App
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
    , _streamLog = do
        uuid <- liftIO UUID4.nextRandom
        pure $ streamHandler cfg uuid jobId
    }

rawLogHandler :: JobId -> Eff App.AppServantStack Text
rawLogHandler jobId = do
  job <- App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  logText <-
    liftIO $ readFileBS $ job.jobWorkingDir </> "output.log"
  pure $ decodeUtf8 logText

data LogChunk = LogChunk Int LogChunkMsg

data LogChunkMsg = Close | Chunk (Html ())

sseType :: LogChunkMsg -> LByteString
sseType Close = "close"
sseType (Chunk _) = "chunk"

sseContent :: LogChunkMsg -> LByteString
sseContent Close = ""
sseContent (Chunk html) = Lucid.renderBS html

instance ToServerEvent LogChunk where
  toServerEvent (LogChunk ident t) =
    ServerEvent
      (Just $ sseType t)
      (Just $ show ident)
      (sseContent t)

streamHandler :: App.AppState -> UUID -> JobId -> S.SourceT IO LogChunk
streamHandler cfg uuid jobId = S.fromStepT $ step 0 True
  where
    step (n :: Int) (initial :: Bool) = S.Effect $ do
      job :: Job <- App.runApp cfg $ do
        App.query (St.GetJobA jobId) >>= maybe (error "WHASUP") pure
      if not initial && not (job.jobStatus == St.JobRunning || job.jobStatus == St.JobPending)
        then do
          putStrLn $ show uuid <> "Closing"
          pure $ S.Yield (LogChunk n Close) $ step (n + 1) False
        else do
          let logFile = job.jobWorkingDir </> "output.log"
          -- TODO:
          -- 1. Read last N lines, rather than reading from scratch
          -- 2. Send chunks, not single line. And delay?
          unless initial $ do
            putStrLn $ show uuid <> " Waiting: " ++ logFile
            liftIO $ threadDelay 1_000_000
          putStrLn $ show uuid <> " Tailing log file: " ++ logFile
          contents <- readFileText logFile
          let tailc = take 12 $ reverse $ lines contents
          pure $ S.Yield (LogChunk n $ Chunk $ pre_ $ toHtml $ unlines tailc) $ step (n + 1) False

-- Function to safely read a line and handle EOF
getLineSafe :: Handle -> IO (Maybe String)
getLineSafe handle = do
  eof <- hIsEOF handle -- Check if we're at EOF
  if eof
    then return Nothing -- Return Nothing if EOF is reached
    else do
      line <- hGetLine handle -- Read the line
      return (Just line) -- Return the line wrapped in Just
        `catch` \e ->
          if isEOFError e -- Catch EOF exception
            then return Nothing
            else ioError e -- Re-throw other IO errors
