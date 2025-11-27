{- | Git remote operations

Provides git remote commands for querying remote repository information.
-}
module Effectful.Git.Command.Remote (
  getRemoteUrl,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Colog.Simple.Process (withLogCommand)
import Effectful.Error.Static (Error)
import Effectful.Git.Core (git)
import Effectful.Process (Process, proc, readCreateProcess)
import Effectful.Reader.Static qualified as ER

{- | Get the URL for a git remote

Gets the URL for the specified remote (typically "origin") in the given repository directory.
-}
getRemoteUrl ::
  ( Error Text :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  -- | Repository directory
  FilePath ->
  -- | Remote name (typically "origin")
  Text ->
  Eff es Text
getRemoteUrl repoDir remoteName = do
  let cmd = proc git ["-C", repoDir, "remote", "get-url", toString remoteName]
  output <- withLogCommand cmd $ do
    log Debug $ "Running git remote get-url " <> remoteName
    readCreateProcess cmd ""
  pure $ T.strip $ toText output
