# nix-shell-cache-lib.sh — shared cache key logic for .envrc and nix-develop-cached
#
# Sources this to get: CACHE_DIR, CACHE_KEY, ENV_FILE, cache_valid()
# Expects DIR to be set to the project root before sourcing.

: "${DIR:="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"}"

CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/vira-shell"

NIX_FILES=(
    "$DIR/flake.nix"
    "$DIR/flake.lock"
    "$DIR/nix/modules/flake-parts/devshell.nix"
    "$DIR/nix/modules/flake-parts/haskell.nix"
    "$DIR/nix/modules/flake-parts/pre-commit.nix"
    "$DIR/nix/modules/flake-parts/assets.nix"
    "$DIR/nix/modules/flake-parts/vira-dev.nix"
    "$DIR/nix/modules/flake-parts/hpack-watch.nix"
    "$DIR/nix/modules/flake-parts/tests.nix"
    "$DIR/packages/attic/haskell-module.nix"
    "$DIR/packages/nix-cache-server/haskell-module.nix"
)

CACHE_KEY=$(cat "${NIX_FILES[@]}" 2>/dev/null | { sha256sum 2>/dev/null || shasum -a 256; } | cut -d' ' -f1)
ENV_FILE="$CACHE_DIR/$CACHE_KEY.sh"

cache_valid() {
    [[ -f "$ENV_FILE" ]] || return 1
    local store_path
    store_path=$(grep -o '/nix/store/[^/"'"'"']*' "$ENV_FILE" | head -1)
    [[ -n "$store_path" ]] && [[ -e "$store_path" ]]
}
