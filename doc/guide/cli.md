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

- `--only-build` / `-b` - Skip cache and signoff stages, only run build for current system

### Default Behavior {#default}

By default, `vira ci` respects the [[config|`vira.hs`]] configuration for all stages:

- Runs build, cache, and signoff stages as configured
- Enables creating per-system signoffs (e.g., `vira/x86_64-linux`) during local development
- Pushes to cache if configured

### Build-Only Mode {#build-only}

Use the `--only-build` flag for quick local testing without side effects:

```bash
# Only build, skip cache and signoff
vira ci --only-build

# Short form
vira ci -b
```

When `--only-build` is used:

- Only runs the build stage
- Ignores `build.systems` from config (uses current system only)
- Skips cache push even if configured
- Skips signoff creation even if configured

### Example

```bash
# Run CI with full configuration (build, cache, signoff)
vira ci

# Run CI in specific directory
vira ci /path/to/repo

# Quick build-only mode
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
