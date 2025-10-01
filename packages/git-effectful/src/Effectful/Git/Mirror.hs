{- | Git repository mirroring with file-based locking.

Maintains a synchronized mirror of a git repository (metadata only, without blobs)
that can be safely accessed and updated by multiple processes. Uses file locks
to serialize concurrent updates to the same mirror.

The mirror is created with @--filter=blob:none@ for efficiency, containing only
repository metadata (refs, commits, trees) without file blobs.

= Usage

@
result <- syncMirror "https://..." "/path/to/mirror"
case result of
  Right () -> -- mirror is up-to-date and ready for use
  Left err -> -- handle error, may need to delete corrupt mirror
@
-}
module Effectful.Git.Mirror (
  syncMirror,
) where

import Colog (Message, Severity (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (finally)
import Effectful.Git (git)
import Effectful.Git.Logging (log)
import Effectful.Process (CreateProcess (..), Process, proc, readCreateProcessWithExitCode)
import Lukko (LockMode (ExclusiveLock))
import Lukko qualified
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName)

{- | Ensure git mirror exists and is up-to-date.

Creates mirror if needed, then fetches latest changes.
Concurrent calls for the same directory block on file lock until the first completes.
On error, caller may need to delete the mirror directory and retry.
-}
syncMirror ::
  ( Error Text :> es
  , Log Message :> es
  , Process :> es
  , IOE :> es
  ) =>
  -- | Clone URL
  Text ->
  -- | Mirror directory path
  FilePath ->
  Eff es ()
syncMirror cloneUrl mirrorPath = do
  ensureMirror cloneUrl mirrorPath
  updateMirror mirrorPath

{- | Clone repository mirror if not present. Idempotent.

Checks directory existence first. If missing, acquires lock and clones.
Race condition: Multiple processes may see "doesn't exist" before first clone completes,
but file lock ensures only one actually clones.
-}
ensureMirror ::
  ( Error Text :> es
  , Log Message :> es
  , Process :> es
  , IOE :> es
  ) =>
  Text ->
  FilePath ->
  Eff es ()
ensureMirror cloneUrl mirrorPath = do
  -- Check if mirror directory exists
  exists <- liftIO $ doesDirectoryExist mirrorPath

  if exists
    then log Info $ "Git mirror already exists: " <> toText mirrorPath
    else do
      -- Clone the repository with file lock protection
      log Info $ "Creating git mirror at " <> toText mirrorPath

      withFileLock mirrorPath $ do
        -- Create parent directory
        liftIO $ createDirectoryIfMissing True (takeDirectory mirrorPath)

        -- Clone the repository
        let cloneCmd = cloneAllBranches cloneUrl (takeFileName mirrorPath)
            parentDir = takeDirectory mirrorPath

        log Info $ "Running git clone: " <> show (cmdspec cloneCmd)

        (exitCode, stdoutStr, stderrStr) <-
          readCreateProcessWithExitCode
            cloneCmd {cwd = Just parentDir}
            ""

        case exitCode of
          ExitSuccess ->
            log Info $ "Successfully created git mirror at " <> toText mirrorPath
          ExitFailure code -> do
            let errorMsg =
                  "Git clone failed with exit code "
                    <> show code
                    <> ". Stdout: "
                    <> toText stdoutStr
                    <> ". Stderr: "
                    <> toText stderrStr
            log Error errorMsg
            throwError errorMsg

{- | Fetch latest changes from remote. Uses @--force@ to handle force-pushes.

Acquires file lock before fetching to prevent concurrent updates to same mirror.
-}
updateMirror ::
  ( Error Text :> es
  , Log Message :> es
  , Process :> es
  , IOE :> es
  ) =>
  FilePath ->
  Eff es ()
updateMirror mirrorPath = do
  log Info $ "Updating git mirror at " <> toText mirrorPath

  withFileLock mirrorPath $ do
    -- Use --force to handle forced pushes
    let fetchCmd = fetchAllBranches

    log Info $ "Running git fetch: " <> show (cmdspec fetchCmd)

    (exitCode, stdoutStr, stderrStr) <-
      readCreateProcessWithExitCode
        fetchCmd {cwd = Just mirrorPath}
        ""

    case exitCode of
      ExitSuccess ->
        log Info $ "Successfully updated git mirror at " <> toText mirrorPath
      ExitFailure code -> do
        let errorMsg =
              "Git fetch failed with exit code "
                <> show code
                <> ". Stdout: "
                <> toText stdoutStr
                <> ". Stderr: "
                <> toText stderrStr
        log Error errorMsg
        throwError errorMsg

{- | Run action with file-based lock on a directory.

Creates/acquires @dirPath.lock@ file.
Blocks if another process holds the lock, ensuring sequential access.
Exception-safe via finally.
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

-- | Return the `CreateProcess` to clone a repo with all branches (blob:none for efficiency)
cloneAllBranches :: Text -> FilePath -> CreateProcess
cloneAllBranches url path =
  proc
    git
    [ "clone"
    , "-v"
    , "--filter=blob:none"
    , "--no-single-branch"
    , toString url
    , path
    ]

-- | Return the `CreateProcess` to fetch all branches with force
fetchAllBranches :: CreateProcess
fetchAllBranches =
  proc
    git
    [ "fetch"
    , "--force"
    , "origin"
    , "+refs/heads/*:refs/remotes/origin/*"
    ]
