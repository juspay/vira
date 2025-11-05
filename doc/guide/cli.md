---
slug: cli
---

# CLI Usage

Vira provides a command-line interface for running CI pipelines locally and managing state.

## Running CI Locally {#ci}

Run the CI pipeline in any directory:

```bash
vira ci [DIRECTORY]
```

If no directory is specified, runs in the current directory. The directory must be a git repository.

### CLI Mode Restrictions {#build-only}

When running CI via the command line, only the build stage will run and for current system only.

This behavior is enforced automatically, regardless of what [[config|`vira.hs`]] specifies.

### Example

```bash
# Run CI in current directory
vira ci

# Run CI in specific directory
vira ci /path/to/repo
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
