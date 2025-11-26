{- | Git remote operations

Provides git remote commands for querying remote repository information.
-}
module Effectful.Git.Command.Remote (
  getRemoteUrl,
) where

import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.Git.Core (git)
import Effectful.Process (Process, proc, readCreateProcess)

{- | Get the URL for a git remote

Gets the URL for the specified remote (typically "origin") in the given repository directory.
-}
getRemoteUrl ::
  ( Error Text :> es
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
  output <- readCreateProcess cmd ""
  pure $ T.strip $ toText output
