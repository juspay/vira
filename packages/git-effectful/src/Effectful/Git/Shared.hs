{-# LANGUAGE OverloadedRecordDot #-}

{- | Shared git clone management with file-based locking.

Maintains a single clone per repository at @baseWorkDir/repoName/source@,
reusing it across operations to avoid repeated cloning. Uses file locks
(@source.lock@) to serialize concurrent updates to the same repository,
working across processes.

= Usage

@
result <- ensureAndUpdateSharedClone "myrepo" "https://..." "/work"
case result of
  Right path -> -- use path for git operations
  Left err -> -- handle error, may need to delete corrupt clone
@
-}
module Effectful.Git.Shared (
  ensureAndUpdateSharedClone,
) where

import Colog (Message, Msg (..), Severity (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog qualified as Log
import Effectful.Git (RepoName (..), cloneShared, git)
import Lukko (LockMode (ExclusiveLock))
import Lukko qualified
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import UnliftIO.Exception (finally)

{- | Ensure shared clone exists and is up-to-date, returning its path.

Clones if needed, then fetches latest. Returns @baseWorkDir/repoName/source@.
Concurrent calls for the same repo block on file lock until the first completes.
On error, caller may need to delete the clone directory and retry.
-}
ensureAndUpdateSharedClone ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  RepoName ->
  -- | Clone URL
  Text ->
  -- | Base work directory
  FilePath ->
  Eff es (Either Text FilePath)
ensureAndUpdateSharedClone repoName cloneUrl baseWorkDir = do
  ensureResult <- ensureSharedClone repoName cloneUrl baseWorkDir
  case ensureResult of
    Left err -> return $ Left err
    Right () -> do
      updateResult <- updateSharedClone repoName baseWorkDir
      case updateResult of
        Left err -> return $ Left err
        Right () -> return $ Right $ getSharedClonePath baseWorkDir repoName

{- | Compute path to shared clone: @baseWorkDir/repoName/source@.

The @source@ subdirectory allows future expansion (e.g., worktrees, metadata).
-}
getSharedClonePath :: FilePath -> RepoName -> FilePath
getSharedClonePath baseWorkDir repoName =
  baseWorkDir </> toString repoName </> "source"

{- | Clone repository if not present. Idempotent.

Checks directory existence first. If missing, acquires lock and clones.
Race condition: Multiple processes may see "doesn't exist" before first clone completes,
but file lock ensures only one actually clones.
-}
ensureSharedClone ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  RepoName ->
  Text ->
  FilePath ->
  Eff es (Either Text ())
ensureSharedClone repoName cloneUrl baseWorkDir = do
  let sharedClonePath = getSharedClonePath baseWorkDir repoName

  -- Check if shared clone directory exists
  exists <- liftIO $ doesDirectoryExist sharedClonePath

  if exists
    then do
      Log.logMsg $
        Msg
          { msgSeverity = Info
          , msgText = "Shared clone already exists: " <> toText sharedClonePath
          , msgStack = callStack
          }
      return $ Right ()
    else do
      -- Clone the repository with file lock protection
      Log.logMsg $
        Msg
          { msgSeverity = Info
          , msgText = "Creating shared clone for " <> repoName.unRepoName
          , msgStack = callStack
          }

      withFileLock (baseWorkDir </> toString repoName </> "source") $ do
        -- Create parent directory
        liftIO $ createDirectoryIfMissing True (baseWorkDir </> toString repoName)

        -- Clone the repository
        let cloneCmd = cloneShared cloneUrl "source"

        Log.logMsg $
          Msg
            { msgSeverity = Info
            , msgText = "Running git clone: " <> show (cmdspec cloneCmd)
            , msgStack = callStack
            }

        (exitCode, stdoutStr, stderrStr) <-
          liftIO $
            readCreateProcessWithExitCode
              cloneCmd {cwd = Just (baseWorkDir </> toString repoName)}
              ""

        case exitCode of
          ExitSuccess -> do
            Log.logMsg $
              Msg
                { msgSeverity = Info
                , msgText = "Successfully created shared clone for " <> repoName.unRepoName
                , msgStack = callStack
                }
            return $ Right ()
          ExitFailure code -> do
            let errorMsg =
                  "Git clone failed with exit code "
                    <> show code
                    <> ". Stdout: "
                    <> toText stdoutStr
                    <> ". Stderr: "
                    <> toText stderrStr
            Log.logMsg $
              Msg
                { msgSeverity = Error
                , msgText = errorMsg
                , msgStack = callStack
                }
            return $
              Left $
                "Failed to create shared clone for "
                  <> repoName.unRepoName
                  <> ". Please delete "
                  <> toText sharedClonePath
                  <> " and try again."

{- | Fetch latest changes from remote. Uses @--force@ to handle force-pushes.

Acquires file lock before fetching to prevent concurrent updates to same repo.
-}
updateSharedClone ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  RepoName ->
  FilePath ->
  Eff es (Either Text ())
updateSharedClone repoName baseWorkDir = do
  let sharedClonePath = getSharedClonePath baseWorkDir repoName

  Log.logMsg $
    Msg
      { msgSeverity = Info
      , msgText = "Updating shared clone for " <> repoName.unRepoName
      , msgStack = callStack
      }

  withFileLock sharedClonePath $ do
    -- Use --force to handle forced pushes
    let fetchCmd =
          proc
            git
            [ "fetch"
            , "--force"
            , "origin"
            , "+refs/heads/*:refs/remotes/origin/*"
            ]

    Log.logMsg $
      Msg
        { msgSeverity = Info
        , msgText = "Running git fetch: " <> show (cmdspec fetchCmd)
        , msgStack = callStack
        }

    (exitCode, stdoutStr, stderrStr) <-
      liftIO $
        readCreateProcessWithExitCode
          fetchCmd {cwd = Just sharedClonePath}
          ""

    case exitCode of
      ExitSuccess -> do
        Log.logMsg $
          Msg
            { msgSeverity = Info
            , msgText = "Successfully updated shared clone for " <> repoName.unRepoName
            , msgStack = callStack
            }
        return $ Right ()
      ExitFailure code -> do
        let errorMsg =
              "Git fetch failed with exit code "
                <> show code
                <> ". Stdout: "
                <> toText stdoutStr
                <> ". Stderr: "
                <> toText stderrStr
        Log.logMsg $
          Msg
            { msgSeverity = Error
            , msgText = errorMsg
            , msgStack = callStack
            }
        return $
          Left $
            "Failed to update shared clone for "
              <> repoName.unRepoName
              <> ". Please delete "
              <> toText sharedClonePath
              <> " and try again."

{- | Run action with file-based lock on a directory.

Creates/acquires @dirPath.lock@ file.
Blocks if another process holds the lock, ensuring sequential access.
Exception-safe via onException.
-}
withFileLock ::
  (IOE :> es) =>
  -- | Directory to lock
  FilePath ->
  Eff es a ->
  Eff es a
withFileLock dirPath action = do
  let lockPath = dirPath <> ".lock"

  -- Acquire lock
  fd <- liftIO $ do
    createDirectoryIfMissing True (takeDirectory lockPath)
    fd <- Lukko.fdOpen lockPath
    Lukko.fdLock fd ExclusiveLock
    return fd

  -- Run action with cleanup via finally
  action `finally` liftIO (Lukko.fdUnlock fd >> Lukko.fdClose fd)
