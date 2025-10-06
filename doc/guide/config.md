---
slug: config
---

# Repository Configuration

> [!warning]
> The `vira.hs` configuration format is experimental and may change in future versions.

Vira supports per-repository configuration through an optional `vira.hs` file placed in the root of your repository. This allows you to customize the CI pipeline for each project individually.

## Configuration File

Create a `vira.hs` file in your repository root:

```haskell
-- vira.hs
\ctx pipeline ->
  pipeline
    { signoff.enable = True
    , build.overrideInputs = [("nixpkgs", "github:nixos/nixpkgs/nixos-unstable")]
    , cache.url = Just "https://attic.example.com/my-cache"
    }
```

The configuration uses Haskell's `OverloadedRecordUpdate` syntax for modifying the pipeline structure. This provides a clean, readable way to update nested record fields using dot notation and record update syntax.

## Configuration DSL

The configuration function receives two parameters:

- `ctx` - The Vira context containing repository and branch information
- `pipeline` - The default pipeline configuration to customize

### Available Pipeline Stages

#### Build Stage

```haskell
pipeline { build.enable = True }
pipeline { build.overrideInputs = [("input-name", "flake-url")] }
```

#### Cache Stage

Configure binary cache pushing to an Attic server:

```haskell
pipeline { cache.url = Just "https://attic.example.com/my-cache" }
pipeline { cache.url = Nothing }  -- Disable cache
```

The cache URL should point to an Attic cache. Make sure you've run `attic login` first.

> [!TIP]
> Only Attic is currently supported for binary caching. Cachix support may be added on a needs basis.

#### Signoff Stage

```haskell
pipeline { signoff.enable = True }
```

## Conditional Configuration

You can customize the pipeline based on branch or repository information:

```haskell
\ctx pipeline ->
  let isMainBranch = ctx.branch == "main"
      isReleaseBranch = "release-" `isPrefixOf` ctx.branch
  in pipeline
    { signoff.enable = not isMainBranch
    , cache.url = if isMainBranch || isReleaseBranch
                  then Just "https://attic.example.com/prod-cache"
                  else Nothing
    , build.overrideInputs = [("local", "github:boolean-option/false") | isReleaseBranch]
    }
```

## Examples

See the [sample configurations](https://github.com/juspay/vira/tree/main/sample-configs) for more examples.

## Behavior

- If no `vira.hs` file is found, Vira uses the default pipeline configuration
- Configuration parsing errors will stop the CI build and display an error message
- The final pipeline configuration is logged for debugging purposes
- Configuration changes take effect immediately on the next build
