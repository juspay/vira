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
    & #signoff % #signoffEnable .~ True
    & #build % #overrideInputs .~ [("nixpkgs", "github:nixos/nixpkgs/nixos-unstable")]
    & #attic % #atticEnable .~ False
```

The configuration uses optics operators (`&`, `%`, `.~`) for modifying the pipeline structure. These operators are automatically imported and available in the configuration context. See the [optics documentation](https://hackage.haskell.org/package/optics) for more details on these operators.

## Configuration DSL

The configuration function receives two parameters:

- `ctx` - The Vira context containing repository and branch information
- `pipeline` - The default pipeline configuration to customize

### Available Pipeline Stages

#### Build Stage

```haskell
pipeline & #build % #buildEnable .~ True
pipeline & #build % #overrideInputs .~ [("input-name", "flake-url")]
```

#### Attic Cache Stage

```haskell
pipeline & #attic % #atticEnable .~ True
```

#### Cachix Cache Stage

```haskell
pipeline & #cachix % #cachixEnable .~ True
```

#### Signoff Stage

```haskell
pipeline & #signoff % #signoffEnable .~ True
```

## Conditional Configuration

You can customize the pipeline based on branch or repository information:

```haskell
\ctx pipeline ->
  let isMainBranch = ctx.branch == "main"
      isReleaseBranch = "release-" `isPrefixOf` ctx.branch
  in pipeline
    & #signoff % #signoffEnable .~ not isMainBranch
    & #attic % #atticEnable .~ (isMainBranch || isReleaseBranch)
    & #build % #overrideInputs .~
        [("local", "github:boolean-option/false") | isReleaseBranch]
```

## Examples

See the [sample configurations](https://github.com/juspay/vira/tree/main/sample-configs) for more examples.

## Behavior

- If no `vira.hs` file is found, Vira uses the default pipeline configuration
- Configuration parsing errors will stop the CI build and display an error message
- The final pipeline configuration is logged for debugging purposes
- Configuration changes take effect immediately on the next build
