#!/usr/bin/env bash
# GHCID script for multi-package development with home-units

set -euo pipefail

# Change to the home-units directory
cd "$(dirname "$0")"

# Start ghcid with home-units enabled
ghcid --command="cabal repl exe:vira" --test=":main --help" --restart=cabal.project