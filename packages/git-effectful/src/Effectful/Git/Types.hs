{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Git types

Core data types for git operations.
-}
module Effectful.Git.Types (
  -- * Types
  Commit (..),
  CommitID (..),
  BranchName (..),
  RepoName (..),
  CommitIxs,
  IxCommit,
) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Data (Data)
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.SafeCopy
import Data.Time (UTCTime)
import Servant (FromHttpApiData, ToHttpApiData)

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
    , ToString
    , ToText
    , ToJSON
    , ToHttpApiData
    , FromHttpApiData
    )

-- | Git branch name
newtype BranchName = BranchName {unBranchName :: Text}
  deriving stock (Generic, Data)
  deriving newtype (Show, Eq, Ord)
  deriving newtype
    ( IsString
    , ToString
    , ToText
    , ToJSON
    , ToHttpApiData
    , FromHttpApiData
    )

-- | Git repository name
newtype RepoName = RepoName {unRepoName :: Text}
  deriving stock (Generic, Data)
  deriving newtype (Show, Eq, Ord)
  deriving newtype
    ( IsString
    , ToText
    , ToString
    , ToHttpApiData
    , FromHttpApiData
    , ToJSON
    , FromJSON
    , ToJSONKey
    , FromJSONKey
    )

$(deriveSafeCopy 0 'base ''CommitID)
$(deriveSafeCopy 0 'base ''BranchName)
$(deriveSafeCopy 0 'base ''RepoName)
$(deriveSafeCopy 0 'base ''Commit)

-- | IxSet index for commits
type CommitIxs = '[CommitID]

type IxCommit = IxSet CommitIxs Commit

instance Indexable CommitIxs Commit where
  indices = ixList (ixFun $ \commit -> [commit.commitId])
