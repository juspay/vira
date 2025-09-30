{-# LANGUAGE OverloadedRecordDot #-}

{- | Module for managing shared git clones with STM concurrency control

This module provides thread-safe management of shared git repositories
to avoid repeated cloning during branch refresh operations.
-}
module Effectful.Git.Shared (
  SharedCloneState,
  newSharedCloneState,
  ensureAndUpdateSharedClone,
) where

import Colog (Message, Msg (..), Severity (..))
import Control.Concurrent.STM qualified as STM
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog qualified as Log
import Effectful.Git (RepoName (..), cloneShared, git)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

-- | State tracking which repositories are currently being updated
newtype SharedCloneState = SharedCloneState (STM.TVar (Map RepoName Bool))

-- | Create a new shared clone state
newSharedCloneState :: IO SharedCloneState
newSharedCloneState = SharedCloneState <$> STM.atomically (STM.newTVar Map.empty)

-- | Ensure a shared clone exists and update it with latest changes, returning the path
ensureAndUpdateSharedClone ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  SharedCloneState ->
  RepoName ->
  Text ->
  FilePath ->
  Eff es (Either Text FilePath)
ensureAndUpdateSharedClone sharedState repoName cloneUrl baseWorkDir = do
  ensureResult <- ensureSharedClone sharedState repoName cloneUrl baseWorkDir
  case ensureResult of
    Left err -> return $ Left err
    Right () -> do
      updateResult <- updateSharedClone sharedState repoName baseWorkDir
      case updateResult of
        Left err -> return $ Left err
        Right () -> return $ Right $ getSharedClonePath baseWorkDir repoName

-- | Get the path to the shared clone for a repository
getSharedClonePath :: FilePath -> RepoName -> FilePath
getSharedClonePath baseWorkDir repoName =
  baseWorkDir </> toString repoName </> "source"

-- | Ensure a shared clone exists for the given repository
ensureSharedClone ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  SharedCloneState ->
  RepoName ->
  Text ->
  FilePath ->
  Eff es (Either Text ())
ensureSharedClone sharedState repoName cloneUrl baseWorkDir = do
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
      -- Clone the repository with STM protection
      Log.logMsg $
        Msg
          { msgSeverity = Info
          , msgText = "Creating shared clone for " <> repoName.unRepoName
          , msgStack = callStack
          }

      withSTMLock sharedState repoName $ do
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

-- | Update an existing shared clone with latest changes from remote
updateSharedClone ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  SharedCloneState ->
  RepoName ->
  FilePath ->
  Eff es (Either Text ())
updateSharedClone sharedState repoName baseWorkDir = do
  let sharedClonePath = getSharedClonePath baseWorkDir repoName

  Log.logMsg $
    Msg
      { msgSeverity = Info
      , msgText = "Updating shared clone for " <> repoName.unRepoName
      , msgStack = callStack
      }

  withSTMLock sharedState repoName $ do
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

-- | Execute an action with STM lock for the given repository
withSTMLock ::
  (IOE :> es) =>
  SharedCloneState ->
  RepoName ->
  Eff es a ->
  Eff es a
withSTMLock (SharedCloneState stateVar) repoName action = do
  -- Acquire lock
  liftIO $ STM.atomically $ do
    stateMap <- STM.readTVar stateVar
    case Map.lookup repoName stateMap of
      Just True -> STM.retry -- Repository is being updated, wait
      _ -> STM.writeTVar stateVar (Map.insert repoName True stateMap)

  -- Execute action
  result <- action

  -- Release lock
  liftIO $ STM.atomically $ do
    stateMap <- STM.readTVar stateVar
    STM.writeTVar stateVar (Map.delete repoName stateMap)

  return result
