{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Module for working with Git repositories in Haskell

A standalone library for git operations using the effectful library.
Servant instances should be gated behind a Cabal flag.
-}
module Effectful.Git where

import Colog (Message, Msg (..), Severity (..))
import Control.Exception (try)
import Data.Aeson (ToJSON)
import Data.Data (Data)
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.Map.Strict qualified as Map
import Data.SafeCopy
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog qualified as Log
import Servant (FromHttpApiData, ToHttpApiData)
import System.Directory (doesDirectoryExist)
import System.Process
import System.Which (staticWhich)
import Text.Megaparsec (Parsec, anySingle, manyTill, parse, takeRest)
import Text.Megaparsec.Char (tab)

{- | Path to the `git` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
git :: FilePath
git = $(staticWhich "git")

-- A git commit object.
data Commit = Commit
  { commitId :: CommitID
  -- ^ The unique identifier of the commit
  , commitMessage :: Text
  -- ^ The commit message
  , commitDate :: UTCTime
  -- ^ The commit date
  , commitAuthor :: Text
  -- ^ The commit author name
  , commitAuthorEmail :: Text
  -- ^ The commit author email
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

-- | Git commit hash
newtype CommitID = CommitID {unCommitID :: Text}
  deriving stock (Generic, Show, Eq, Ord, Data)
  deriving newtype
    ( IsString
    , ToJSON
    , ToString
    , ToHttpApiData
    , FromHttpApiData
    )

-- | Git branch name
newtype BranchName = BranchName {unBranchName :: Text}
  deriving stock (Generic, Data)
  deriving newtype (Show, Eq, Ord)
  deriving newtype
    ( IsString
    , ToJSON
    , ToString
    , ToHttpApiData
    , FromHttpApiData
    )

$(deriveSafeCopy 0 'base ''CommitID)
$(deriveSafeCopy 0 'base ''BranchName)
$(deriveSafeCopy 0 'base ''Commit)

-- | IxSet index for commits
type CommitIxs = '[CommitID]

type IxCommit = IxSet CommitIxs Commit

instance Indexable CommitIxs Commit where
  indices = ixList (ixFun $ \commit -> [commit.commitId])

-- | Parse a git ref line into a branch name and commit
gitRefParser :: Parsec Void Text (BranchName, Commit)
gitRefParser = do
  branchName' <- toText <$> manyTill anySingle tab
  commitId <- fromString <$> manyTill anySingle tab
  timestampStr <- manyTill anySingle tab
  author <- toText <$> manyTill anySingle tab
  authorEmailRaw <- toText <$> manyTill anySingle tab
  message <- takeRest

  -- Strip "origin/" prefix from branch name if present to get clean branch names
  let branchName = fromString . toString $ T.stripPrefix "origin/" branchName' ?: branchName'

  -- Strip angle brackets from email if present (git %(authoremail) includes < >)
  let authorEmail = T.strip $ fromMaybe authorEmailRaw $ do
        stripped1 <- T.stripPrefix "<" authorEmailRaw
        T.stripSuffix ">" stripped1

  timestamp <- maybe (fail $ "Invalid timestamp: " <> timestampStr) return (readMaybe timestampStr)
  let date = posixSecondsToUTCTime (fromIntegral (timestamp :: Int))

  let commit = Commit commitId message date author authorEmail
  return (branchName, commit)

-- | Return the `CreateProcess` to clone a repo at a specific commit
cloneAtCommit :: Text -> CommitID -> FilePath -> CreateProcess
cloneAtCommit url commit path =
  proc
    git
    [ "-c"
    , "advice.detachedHead=false"
    , "clone"
    , "--depth"
    , "1"
    , "--single-branch"
    , "--revision"
    , toString commit
    , toString url
    , path
    ]

-- | Return the `CreateProcess` to clone from a shared clone at a specific commit
cloneFromSharedClone :: FilePath -> CommitID -> FilePath -> CreateProcess
cloneFromSharedClone sharedClonePath commit targetPath =
  proc
    git
    [ "-c"
    , "advice.detachedHead=false"
    , "clone"
    , "--depth"
    , "1"
    , "--single-branch"
    , "--revision"
    , toString commit
    , sharedClonePath
    , targetPath
    ]

{- | Get remote branches using a shared clone directory.
This function expects the shared clone to already exist and be updated.
It parses branches from the existing shared clone without modifying it.
-}
remoteBranchesFromSharedClone :: (Log Message :> es, IOE :> es) => FilePath -> Eff es (Either Text (Map BranchName Commit))
remoteBranchesFromSharedClone sharedClonePath = do
  -- Check if shared clone directory exists
  exists <- liftIO $ doesDirectoryExist sharedClonePath

  if not exists
    then do
      Log.logMsg $
        Msg
          { msgSeverity = Error
          , msgText = "Shared clone directory does not exist: " <> toText sharedClonePath
          , msgStack = callStack
          }
      return $ Left $ "Shared clone directory does not exist: " <> toText sharedClonePath
    else do
      -- Use git for-each-ref to get detailed branch information for remote branches only
      let forEachRefCmd =
            proc
              git
              [ "for-each-ref"
              , "--format=%(refname:short)%09%(objectname)%09%(committerdate:unix)%09%(authorname)%09%(authoremail)%09%(subject)"
              , "refs/remotes"
              ]

      Log.logMsg $
        Msg
          { msgSeverity = Info
          , msgText = "Running git for-each-ref in shared clone: " <> show (cmdspec forEachRefCmd)
          , msgStack = callStack
          }

      result <-
        liftIO $
          try $
            readCreateProcess
              forEachRefCmd {cwd = Just sharedClonePath}
              ""

      case result of
        Left (ex :: SomeException) -> do
          let errorMsg = "Git for-each-ref failed: " <> show ex
          Log.logMsg $
            Msg
              { msgSeverity = Error
              , msgText = errorMsg
              , msgStack = callStack
              }
          return $ Left $ toText errorMsg
        Right output -> do
          -- Drop the first line, which is 'origin' (not a branch)
          let gitRefLines = drop 1 $ lines $ T.strip (toText output)
          commits <- catMaybes <$> mapM parseCommitLineShared gitRefLines
          return $ Right $ Map.fromList commits
  where
    parseCommitLineShared :: (Log Message :> es) => Text -> Eff es (Maybe (BranchName, Commit))
    parseCommitLineShared line = case parse gitRefParser "" line of
      Left err -> do
        Log.logMsg $
          Msg
            { msgSeverity = Error
            , msgText = "Parse error on line '" <> line <> "': " <> toText @String (show err)
            , msgStack = callStack
            }
        return Nothing
      Right result -> return $ Just result
