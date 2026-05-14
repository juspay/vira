---
slug: cli
---

# CLI Usage

Vira provides a command-line interface for running CI pipelines locally and managing state.

## Running CI Locally {#ci}

Run the CI pipeline in any directory:

```bash
vira ci [DIRECTORY] [OPTIONS]
```

If no directory is specified, runs in the current directory. The directory must be a git repository.

### Options {#ci-opts}

- `--local` / `-l` - Build only for the current system, run all stages
- `--only-build` / `-b` - Build only for the current system, skip cache and signoff stages
- `--post-build-hook PATH` - Path to a shell script to run after a successful pipeline

The mode flags (`--local`, `--only-build`) are mutually exclusive. `--post-build-hook` can be combined with any mode (but is ignored under `--only-build`).

### Default Behavior {#default}

By default, `vira ci` respects the [[config|`vira.hs`]] configuration for all stages:

- Fails if working directory has uncommitted changes or untracked files
- Runs build, cache, and signoff stages as configured
- Builds for all configured `build.systems`
- Enables creating per-system signoffs (e.g., `vira/x86_64-linux`) during local development
- Pushes to cache if configured
- Runs the script passed via `--post-build-hook` (if any) after a successful pipeline, with `VIRA_BRANCH`, `VIRA_COMMIT_ID`, and `VIRA_REPO_CLONE_URL` exported in the environment

### Local Mode {#local}

Use `--local` to build only for the current system while still running all pipeline stages (cache, signoff). This is useful when remote builders are unreliable and you want to complete CI locally — for example, SSH into a machine and run `vira ci --local` to finish CI for that system all the way through signoff.

```bash
vira ci --local

# Short form
vira ci -l
```

When `--local` is used:

- Ignores `build.systems` from config (uses current system only)
- Runs all stages: build, cache, and signoff

### Build-Only Mode {#build-only}

Use `--only-build` for quick local testing without side effects:

```bash
vira ci --only-build

# Short form
vira ci -b
```

When `--only-build` is used:

- Allows running on dirty working directory (uncommitted changes or untracked files)
- Only runs the build stage
- Ignores `build.systems` from config (uses current system only)
- Skips cache push even if configured
- Skips signoff creation even if configured
- Skips the post-build hook even if `--post-build-hook` is provided

### Post-Build Hook {#post-build-hook}

When `--post-build-hook PATH` is set, vira execs the script after a successful pipeline (skipped under `--only-build`). The script receives three environment variables:

| Variable              | Description                                                    |
| --------------------- | -------------------------------------------------------------- |
| `VIRA_BRANCH`         | Branch being built                                             |
| `VIRA_COMMIT_ID`      | Full commit SHA being built                                    |
| `VIRA_REPO_CLONE_URL` | Origin clone URL — match exactly to avoid cross-org collisions |

A non-zero exit fails the pipeline. The script body is the operator's integration plane — match on `VIRA_REPO_CLONE_URL` for exact dispatch, then branch on `VIRA_BRANCH` for per-branch routing.

```bash
#!/usr/bin/env bash
# /etc/vira/post-build.sh
set -euo pipefail

short_sha="${VIRA_COMMIT_ID:0:7}"

case "$VIRA_REPO_CLONE_URL" in
  https://github.com/juspay/vira.git|git@github.com:juspay/vira.git)
    case "$VIRA_BRANCH" in
      main)
        # Trigger downstream Jenkins integration job
        curl -fsS --retry 3 -X POST \
          -u "$JENKINS_USER:$JENKINS_TOKEN" \
          "https://jenkins.example/job/vira-integration/buildWithParameters?BRANCH=${VIRA_BRANCH}&COMMIT=${VIRA_COMMIT_ID}"
        ;;
      release-*)
        # Announce releases in Slack
        curl -fsS -X POST \
          -H "Content-Type: application/json" \
          -d "{\"text\": \":rocket: vira@${VIRA_BRANCH} (${short_sha}) shipped\"}" \
          "$SLACK_WEBHOOK_URL"
        ;;
    esac
    ;;
esac
```

In a NixOS / home-manager deployment, write the script body inline via `services.vira.postBuildHook` — the module wraps it with `pkgs.writeShellScript` and passes the store path to `--post-build-hook` for you.

### Examples

```bash
# Run full CI (all systems, all stages)
vira ci

# Run CI in specific directory
vira ci /path/to/repo

# Local system only, all stages (skip remote builders)
vira ci -l

# Quick build-only mode (no cache, no signoff)
vira ci -b

# Run CI with a post-build hook
vira ci --post-build-hook /etc/vira/post-build.sh
```

## Export/Import State {#import-export}

Export Vira state to JSON:

```bash
vira export > state.json
```

Import Vira state from JSON:

```bash
vira import < state.json
```

## Show Information {#info}

Display Vira version and schema information:

```bash
vira info
```

## Global Options {#opts}

All commands support these global options:

- `--state-dir DIR` - Directory for storing Vira state (default: `./state`)
- `--log-level LEVEL` - Minimum log severity: Debug, Info, Warning, Error (default: Info)
- `--auto-reset-state` - Automatically reset state on schema mismatch

## Web Server Options {#web-opts}

When running `vira web`, these additional options are available:

- `--host HOST` - Host to bind the HTTP server to (default: `0.0.0.0`)
- `--port PORT` - Port to bind the HTTP server to (default: `5005`)
- `--base-path PATH` - Base URL path for the HTTP server (default: `/`)
- `--no-https` - Disable HTTPS (use HTTP only)
- `--import FILE` - Import JSON file on startup
- `--max-concurrent-builds COUNT` - Maximum concurrent CI builds (defaults to 2)
- `--auto-build-new-branches` - Auto-build new branches (default: only auto-build branches built at least once)
- `--job-retention-days DAYS` - Delete jobs older than N days (default: 14, set to 0 to disable cleanup). See [[cleanup]] for details.
- `--post-build-hook PATH` - Path to a shell script run after each successful pipeline. The script receives `VIRA_BRANCH`, `VIRA_COMMIT_ID`, and `VIRA_REPO_CLONE_URL` in the environment. In NixOS / home-manager deployments this is wired up automatically from `services.vira.postBuildHook` (the operator writes the script body inline; the module wraps it with `pkgs.writeShellScript`).
