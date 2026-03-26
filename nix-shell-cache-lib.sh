# nix-shell-cache-lib.sh — shared cache key logic for .envrc and nix-develop-cached
#
# Sources this to get: CACHE_DIR, CACHE_KEY, ENV_FILE, cache_valid()
# Expects DIR to be set to the project root before sourcing.

: "${DIR:="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"}"

CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/vira-shell"

# nixos-unified autowires all .nix files under nix/modules/flake-parts/,
# so we glob rather than hardcoding to catch new modules automatically.
NIX_FILES=(
    "$DIR/flake.nix"
    "$DIR/flake.lock"
    "$DIR"/nix/modules/flake-parts/*.nix
    "$DIR"/packages/*/haskell-module.nix
)

# Include cabal project file and all package.yaml files in the cache key.
# Haskell dependency changes (package.yaml → .cabal) affect the devShell
# but aren't captured by .nix files alone.
CABAL_FILES=("$DIR/cabal.project")
while IFS= read -r -d '' f; do
    CABAL_FILES+=("$f")
done < <(find "$DIR/packages" -name "package.yaml" -print0 2>/dev/null)

CACHE_KEY=$(cat "${NIX_FILES[@]}" "${CABAL_FILES[@]}" 2>/dev/null | { sha256sum 2>/dev/null || shasum -a 256; } | cut -d' ' -f1)
ENV_FILE="$CACHE_DIR/$CACHE_KEY.sh"

cache_valid() {
    [[ -f "$ENV_FILE" ]] || return 1
    local store_path
    store_path=$(grep -o '/nix/store/[^/"'"'"']*' "$ENV_FILE" | head -1)
    [[ -n "$store_path" ]] && [[ -e "$store_path" ]]
}
