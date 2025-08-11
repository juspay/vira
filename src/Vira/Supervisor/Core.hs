module Vira.Supervisor.Core (
  newSupervisor,
) where

import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Directory qualified
import System.FilePath ((</>))
import Vira.Supervisor.Type

newSupervisor :: (MonadIO m) => m TaskSupervisor
newSupervisor = do
  tasks <- newMVar mempty
  pwd <- liftIO getCurrentDirectory
  workDir <- liftIO $ makeAbsolute $ pwd </> "state" </> "workspace" -- keep it alongside acid-state db
  liftIO $ System.Directory.createDirectoryIfMissing True workDir
  pure $ TaskSupervisor tasks workDir
