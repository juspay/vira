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
    , build.flakes = ["." { overrideInputs = [("nixpkgs", "github:nixos/nixpkgs/nixos-unstable")] }]
    , cache.url = Just "https://attic.example.com/my-cache"
    , postBuild.timeoutSeconds = 120
    }
```

The configuration uses Haskell's `OverloadedRecordUpdate` syntax for modifying the pipeline structure. This provides a clean, readable way to update nested record fields using dot notation and record update syntax.

## Configuration DSL

The configuration function receives two parameters:

- `ctx` - The Vira context containing repository and branch information
  - `ctx.branch` - Current branch name
  - `ctx.onlyBuild` - True when running in build-only mode (e.g., `vira ci --only-build`)
- `pipeline` - The default pipeline configuration to customize

All of [relude](https://hackage.haskell.org/package/relude)'s functions are made available in scope.

### Available Pipeline Stages

#### Build Stage

The build stage accepts a non-empty list of flakes to build. Each flake can specify a path and optional input overrides.

By default, Vira builds a single flake at the repository root (`"."`) for the current system only.

```haskell
-- Build a single flake at the current directory
pipeline { build.flakes = ["."] }

-- Build multiple flakes
pipeline { build.flakes = [".", "./doc", "./examples"] }

-- Build a flake with input overrides
pipeline { build.flakes = ["." { overrideInputs = [("input-name", "flake-url")] }] }

-- Build multiple flakes, some with overrides
pipeline
  { build.flakes =
      [ "."
      , "./doc"
      , "./examples" { overrideInputs = [("vira", ".")] }
      ]
  }

-- Build for multiple systems
pipeline { build.systems = ["x86_64-linux", "aarch64-darwin"] }
```

The flakes use Haskell's `IsString` instance, allowing simple string literals for paths. Use record update syntax to add overrides to specific flakes.

The `systems` field controls which Nix systems to build for. When set to an empty list (default), Vira builds only for the current system. When specified, it uses [nix-systems](https://github.com/srid/nix-systems) to override the `systems` flake input.

> [!TIP]
> Building for multiple systems requires [[remote-builds]] to be configured. See the [[remote-builds]] guide for details.

##### Nix Options

Configure Nix `--option` flags for all build commands via `nix.options`:

```haskell
-- Use relaxed sandbox for builds that need network access
pipeline { nix.options = [("sandbox", "relaxed")] }

-- Set multiple options
pipeline
  { nix.options =
      [ ("sandbox", "relaxed")
      , ("cores", "4")
      , ("max-jobs", "2")
      , ("allow-import-from-derivation", "true")
      ]
  }
```

Only the following option keys are allowed:

| Key                            | Description         | Example value |
| ------------------------------ | ------------------- | ------------- |
| `sandbox`                      | Nix sandbox mode    | `"relaxed"`   |
| `cores`                        | CPU cores per build | `"4"`         |
| `max-jobs`                     | Parallel build jobs | `"2"`         |
| `allow-import-from-derivation` | IFD control         | `"true"`      |

Using any other key will cause the pipeline to fail with an error.

> [!WARNING]
> Only safe, non-secret options are allowed. Secrets (like `access-tokens`) belong in `nix.conf` on the CI machine, not in `vira.hs`.

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

Enables commit status reporting to GitHub or Bitbucket. When enabled, Vira posts build status (successful-only for now) to commits.

- For GitHub, uses GitHub API with token from `gh` CLI.
- For Bitbucket, uses Bitbucket API with token from `bb` CLI.

#### PostBuild Stage

Vira supports a convention-based post-build hook. If a file named `viraPostBuildHook.sh` exists at the **root of your repository** and is executable, it is automatically run after a successful build (after cache push and signoff).

No configuration is needed to enable the hook — its presence in the repo is the trigger.

The only configurable option is the timeout:

```haskell
-- Increase timeout for slow notification scripts (default: 600 seconds)
pipeline { postBuild.timeoutSeconds = 120 }
```

**Creating the hook script:**

```bash
#!/usr/bin/env bash
# viraPostBuildHook.sh — placed at repo root, must be executable
# chmod +x viraPostBuildHook.sh

# Trigger a Jenkins build using credentials from the CI machine environment.
# JENKINS_URL, JENKINS_USER, and JENKINS_TOKEN must be set as env vars on the CI machine.
# Adjust the job path to match your Jenkins folder/job structure.
curl -X POST \
  "${JENKINS_URL}/job/MyOrg/job/my-repo/job/my-job/build?delay=0sec" \
  --user "${JENKINS_USER}:${JENKINS_TOKEN}"
```

Make it executable before committing:

```bash
chmod +x viraPostBuildHook.sh
```

Vira context is exposed as environment variables:

| Environment Variable | Description                                        | Example                        |
| -------------------- | -------------------------------------------------- | ------------------------------ |
| `VIRA_BRANCH`        | Current branch name                                | `main`                         |
| `VIRA_COMMIT_ID`     | Commit ID being built                              | `abc123...`                    |
| `VIRA_CLONE_URL`     | Repository clone URL (empty string if unavailable) | `git@github.com:user/repo.git` |
| `VIRA_REPO_DIR`      | Repository working directory                       | `/workspace/project`           |
| `VIRA_ONLY_BUILD`    | Whether running in build-only mode                 | `true` or `false`              |

> [!NOTE]
> The script is executed directly (not via shell), from the repository root directory. The post-build hook runs **last** — after build, cache push, and signoff. If the hook fails (non-zero exit code) or times out, the pipeline fails. The hook is skipped when running with `--only-build`.

> [!TIP]
> For secrets (API tokens, credentials), set them as environment variables on the CI machine rather than committing them to the repository. The script can access them alongside the Vira context variables.

> [!WARNING]
> The hook script runs with full CI machine privileges and has access to **all environment variables** on the CI machine, including any secrets. Treat `viraPostBuildHook.sh` with the same scrutiny as any other privileged CI configuration.

## Conditional Configuration {#cond}

You can customize the pipeline based on branch or repository information:

```haskell
\ctx pipeline ->
  let isMainBranch = ctx.branch == "main"
      isReleaseBranch = "release-" `isPrefixOf` toString ctx.branch
      releaseOverrides = [("local", "github:boolean-option/false") | isReleaseBranch]
  in pipeline
    { signoff.enable = not isMainBranch
    , cache.url = if isMainBranch || isReleaseBranch
                  then Just "https://attic.example.com/prod-cache"
                  else Nothing
    , build.flakes = ["." { overrideInputs = releaseOverrides }]
    }
```

## Examples

See the [sample configurations](https://github.com/juspay/vira/tree/main/sample-configs) for more examples.

## Behavior

- If no `vira.hs` file is found, Vira uses the default pipeline configuration
- Configuration parsing errors will stop the CI build and display an error message
- The final pipeline configuration is logged for debugging purposes
- Configuration changes take effect immediately on the next build
