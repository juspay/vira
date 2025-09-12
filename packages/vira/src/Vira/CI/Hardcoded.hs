{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Vira.CI.Hardcoded (
  eulerLspConfiguration,
  viraConfiguration,
) where

import Text.RawString.QQ
import Vira.CI.Configuration qualified as Config
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Types (ViraPipeline (..))

-- | Euler LSP configuration using hardcoded YAML
eulerLspConfiguration :: ViraEnvironment -> ViraPipeline -> ViraPipeline
eulerLspConfiguration env basePipeline =
  let yamlConfig =
        [r|
- if:
    branch:
      - "release-*"
  pipeline:
    build:
      overrideInputs:
        "flake/local": "github:boolean-option/false"
|]
   in case Config.parseViraConfig yamlConfig of
        Left err -> error $ "Failed to parse hardcoded euler-lsp YAML config: " <> toText err
        Right config -> Config.applyConfig env basePipeline config

-- | Vira configuration using hardcoded YAML
viraConfiguration :: ViraEnvironment -> ViraPipeline -> ViraPipeline
viraConfiguration env basePipeline =
  let yamlConfig =
        [r|
- pipeline:
    signoff: {}
|]
   in case Config.parseViraConfig yamlConfig of
        Left err -> error $ "Failed to parse hardcoded vira YAML config: " <> toText err
        Right config -> Config.applyConfig env basePipeline config
