{- | Git remote operations

Provides git remote commands for querying remote repository information.
-}
module Effectful.Git.Command.Remote (
  getRemotes,
  getRemoteUrl,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Colog.Simple.Process (withLogCommand)
import Effectful.Git.Core (git)
import Effectful.Process (Process, proc, readCreateProcess)
import Effectful.Reader.Static qualified as ER

-- | List all configured remote names for a repository.
getRemotes ::
  ( Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  -- | Repository directory
  FilePath ->
  Eff es [Text]
getRemotes repoDir = do
  let cmd = proc git ["-C", repoDir, "remote"]
  output <- withLogCommand cmd $ do
    log Debug "Listing git remotes"
    readCreateProcess cmd ""
  pure $ lines $ T.strip $ toText output

{- | Get the URL for a git remote

Gets the URL for the specified remote (typically "origin") in the given repository directory.
Returns 'Nothing' if the remote does not exist.
-}
getRemoteUrl ::
  ( Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  -- | Repository directory
  FilePath ->
  -- | Remote name (typically "origin")
  Text ->
  Eff es (Maybe Text)
getRemoteUrl repoDir remoteName = do
  remotes <- getRemotes repoDir
  if remoteName `elem` remotes
    then do
      let urlCmd = proc git ["-C", repoDir, "remote", "get-url", toString remoteName]
      output <- withLogCommand urlCmd $ do
        log Debug $ "Running git remote get-url " <> remoteName
        readCreateProcess urlCmd ""
      pure $ Just $ T.strip $ toText output
    else do
      log Debug $ "Remote '" <> remoteName <> "' not found"
      pure Nothing
