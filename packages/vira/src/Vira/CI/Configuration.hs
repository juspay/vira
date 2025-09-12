{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.Configuration (
  ViraConfig (..),
  ViraPipelineOverlay (..),
  Condition (..),
  parseViraConfig,
  applyConfig,
  matchesCondition,
) where

import Data.Aeson
import Data.Yaml qualified as Yaml
import Effectful.Git (BranchName (..))
import System.FilePattern (FilePattern, (?==))
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Types (
  ViraPipeline (..),
  mergePipelines,
 )
import Vira.State.Type (Branch (..))

{- | Main configuration for vira.yaml - just a list of conditional
 pipelines that overlay each other
-}
newtype ViraConfig = ViraConfig [ViraPipelineOverlay]
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON)

-- | ViraPipeline with conditions
data ViraPipelineOverlay = ViraPipelineOverlay
  { condition :: Maybe Condition
  , pipeline :: ViraPipeline
  }
  deriving stock (Show, Generic, Eq)

instance FromJSON ViraPipelineOverlay where
  parseJSON = withObject "ViraPipelineOverlay" $ \o -> do
    ifCondition <- o .:? "if"
    pipeline <- o .: "pipeline"
    pure $ ViraPipelineOverlay ifCondition pipeline

-- | Condition set for when a rule should apply
newtype Condition = Condition
  { -- Branch matches one of
    branch :: [FilePattern] -- e.g., ["main", "release/*"]
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON)

-- | Parse YAML configuration from ByteString
parseViraConfig :: ByteString -> Either String ViraConfig
parseViraConfig = Yaml.decodeEither' >>> first show

-- | Apply configuration rules starting with a base pipeline
applyConfig :: ViraEnvironment -> ViraPipeline -> ViraConfig -> ViraPipeline
applyConfig env basePipeline =
  foldl' mergePipelines basePipeline . rulesForEnv
  where
    rulesForEnv :: ViraConfig -> [ViraPipeline]
    rulesForEnv (ViraConfig rules) = flip mapMaybe rules $ \rule -> do
      guard $ maybe True (matchesCondition env) rule.condition
      pure rule.pipeline

-- | Check if environment matches the condition set
matchesCondition :: ViraEnvironment -> Condition -> Bool
matchesCondition env cond =
  matchesBranch env.branch.branchName `any` cond.branch
  where
    -- Pattern matching for branch names using filepattern library
    matchesBranch :: BranchName -> FilePattern -> Bool
    matchesBranch branchName p =
      p ?== toString branchName
