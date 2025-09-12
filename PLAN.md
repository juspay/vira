# Vira CI Configuration Plan

## Overview

This document outlines the plan for implementing `vira.yaml` configuration support for customizing CI pipelines per repository.

## Example YAML Configuration

```yaml
# Default rule (applies to all branches)
- pipeline:
    signoff: # Enable signoff with defaults

# Production branches override
- if:
    branches:
      - "main"
      - "release/*"
  pipeline:
    build:
      overrideInputs:
        local: "github:boolean-option/false"
    attic: # Enable attic with defaults
```

## Haskell Types Design

### Core Configuration Types

```haskell
-- | Main configuration for vira.yaml - just a list of rules
newtype ViraConfig = ViraConfig [ViraPipelineOverlay]
  deriving (Show, Generic)

-- | A configuration rule - ViraPipeline with conditions
data ViraPipelineOverlay = ViraPipelineOverlay
  { if :: Maybe Condition
  , pipeline :: ViraPipeline
  } deriving (Show, Generic)

-- | Condition set for when a rule should apply
data Condition = Condition
  { branches :: [FilePattern]  -- e.g., ["main", "release/*"]
  } deriving (Show, Generic)
```

**Modified Pipeline Types (in Pipeline.hs):**

```haskell
-- | Pipeline with optional stages (Nothing = disabled, Just = enabled)
data ViraPipeline = ViraPipeline
  { build :: Maybe BuildStage
  , attic :: Maybe AtticStage
  , cachix :: Maybe CachixStage
  , signoff :: Maybe SignoffStage
  }
  deriving stock (Generic)

-- | Build stage with optional settings
data BuildStage = BuildStage
  { overrideInputs :: Maybe [(Text, Text)]  -- Nothing = use defaults
  }
  deriving stock (Generic)

-- | Other stages become simple unit types or keep existing optional fields
data AtticStage = AtticStage deriving stock (Generic)
data CachixStage = CachixStage deriving stock (Generic)
data SignoffStage = SignoffStage deriving stock (Generic)
```

### Key Design Decisions

1. **Reuse Pipeline Types Completely**: `ViraPipelineOverlay` directly reuses the exact same stage types from `ViraPipeline`, achieving perfect type reuse.

2. **Maybe-Based Enable/Disable**: Stage presence is controlled by `Maybe` - `Nothing` = disabled, `Just` = enabled. No redundant "enable" fields needed.

3. **Optional Stage Settings**: Within each stage, all settings are `Maybe` to support partial configuration and defaults.

4. **Rule-Based Overlay System**: Configuration uses an overlay approach where rules are applied in order, with later rules overriding earlier ones when conditions match.

5. **Intuitive YAML Syntax**:
   - `stage:` = enable with defaults
   - omit stage = disabled (use defaults from pipeline)
   - `stage: { setting: value }` = enable with specific settings

## Example YAML Configuration

```yaml
rules:
  # Default rule (applies to all branches)
  - pipelineOverrides:
      buildOverrides:
        buildEnable: true
        overrideInputs: []
      signoffOverrides:
        signoffEnable: true

  # Production branches override
  - if:
      branches:
        - "main"
        - "release/*"
    pipelineOverrides:
      buildOverrides:
        overrideInputs:
          - ["local", "github:boolean-option/false"]
      atticOverrides:
        atticEnable: true
      signoffOverrides:
        signoffEnable: false
```

## Implementation Notes

- Rules are applied in order, later rules override earlier ones when conditions match
- YAML will map directly to existing pipeline types via existing lens operations
- This replaces the current hardcoded repository-specific logic in `hardcodePerRepoConfig`
- **All YAML configuration types and parsing logic should be implemented in `Vira.CI.Configuration` module**

## Related Issues

- GitHub Issue: https://github.com/juspay/vira/issues/59
- Related PR: https://github.com/juspay/vira/pull/83
