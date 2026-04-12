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

These flags are mutually exclusive.

### Default Behavior {#default}

By default, `vira ci` respects the [[config|`vira.hs`]] configuration for all stages:

- Fails if working directory has uncommitted changes or untracked files
- Runs build, cache, and signoff stages as configured
- Builds for all configured `build.systems`
- Enables creating per-system signoffs (e.g., `vira/x86_64-linux`) during local development
- Pushes to cache if configured

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
