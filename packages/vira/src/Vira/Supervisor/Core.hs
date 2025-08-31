module Vira.Supervisor.Core (
  newSupervisor,
) where

import System.Directory (makeAbsolute)
import System.Directory qualified
import System.FilePath ((</>))
import Vira.Supervisor.Type

newSupervisor :: (MonadIO m) => FilePath -> m TaskSupervisor
newSupervisor stateDir = do
  tasks <- newMVar mempty
  workDir <- liftIO $ makeAbsolute $ stateDir </> "workspace" -- keep it alongside acid-state db
  liftIO $ System.Directory.createDirectoryIfMissing True workDir
  pure $ TaskSupervisor tasks workDir
