# Home Units for Multi-Package Development

This directory contains configuration for using GHC's home-units feature with ghcid for faster multi-package development.

## Usage

From this directory, run:

```bash
# Start ghcid with multi-package support
./ghcid.sh

# Or manually:
cabal repl exe:vira
```

## Benefits

- Faster compilation when working across multiple packages
- Better integration between packages during development
- Reduced rebuild times when making changes across package boundaries

## Configuration

- `cabal.project`: Main configuration file for the multi-package setup
- `ghcid.sh`: Convenience script to start ghcid with the right settings

The home-units feature allows GHC to treat multiple packages as if they were modules within the same package, enabling faster incremental compilation and better cross-package optimization during development.