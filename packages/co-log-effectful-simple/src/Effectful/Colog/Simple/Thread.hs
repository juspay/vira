{- | Thread labeling utilities for logging.

>>> import Effectful.Colog.Simple.Thread
-}
module Effectful.Colog.Simple.Thread (
  tagCurrentThread,
  threadDesc,
  defaultThreadLabel,
) where

import Data.Text qualified as T
import GHC.Conc (ThreadId, labelThread, myThreadId)
import GHC.Conc.Sync (threadLabel)

-- | Add a label to the current thread
tagCurrentThread :: (MonadIO m) => String -> m ()
tagCurrentThread tag = do
  tid <- liftIO myThreadId
  liftIO $ labelThread tid tag

{- | A short descriptive identifier for 'ThreadId'

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
