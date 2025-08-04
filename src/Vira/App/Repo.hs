{-# LANGUAGE OverloadedRecordDot #-}

module Vira.App.Repo where

import Vira.Lib.Git (BranchName)

newtype RepoSettings = RepoSettings
  { branchSettings :: [BranchSettings]
  }
  deriving stock (Show)

data BranchSettings = BranchSettings
  { branchName :: BranchName
  , buildExtraArgs :: [Text]
  }
  deriving stock (Show)

-- Get the first matching branch setting for a branch name
getBranchSetting :: BranchName -> [BranchSettings] -> Maybe BranchSettings
getBranchSetting branchName branchSettings =
  case filterBranchSettings branchSettings of
    [] -> Nothing
    (x : _) -> Just x
  where
    filterBranchSettings :: [BranchSettings] -> [BranchSettings]
    filterBranchSettings =
      filter
        ( \settings -> matchWildcard (toString settings.branchName) (toString branchName)
        )
    -- \| `matchWildcard pattern input` returns true if `input` matches the wildcard/s in `pattern`
    matchWildcard :: String -> String -> Bool
    matchWildcard = go
      where
        go [] [] = True
        go ('*' : ps) is = any (\n -> go ps (drop n is)) [0 .. length is]
        go (p : ps) (i : is) = p == i && go ps is
        go _ _ = False
