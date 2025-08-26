#!/usr/bin/env bash
set -euo pipefail

# Script to generate Iconoir Haskell bindings
# Usage: ./scripts/generate-iconoir-bindings.sh

ICONOIR_PATH="${ICONOIR_PATH:-$(nix eval --raw .#iconoir.outPath)}"
OUTPUT_DIR="packages/iconoir-hs/src/Web/Iconoir"

echo "Generating Iconoir bindings..."
echo "Iconoir path: $ICONOIR_PATH"
echo "Output directory: $OUTPUT_DIR"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Generate bindings using our Haskell tool
if ! command -v iconoir-codegen &> /dev/null; then
    echo "Building iconoir-codegen first..."
    nix develop -c cabal build iconoir-codegen
    CODEGEN_PATH=$(nix develop -c cabal list-bin iconoir-codegen)
else
    CODEGEN_PATH="iconoir-codegen"
fi

echo "Running code generator..."
nix develop -c "$CODEGEN_PATH" "$ICONOIR_PATH" "$OUTPUT_DIR"

echo "Generated Iconoir bindings successfully!"
echo "Files created:"
ls -la "$OUTPUT_DIR"