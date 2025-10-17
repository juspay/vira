default:
    @just --list

# Run hoogle
[group('2. haskell')]
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run the web application (along with tailwind compilation).
[group('1. vira')]
run:
    nix run .#vira-dev -- --no-server --tui=false

# Like `run`, but only runs Vira, taking optional set of args (web, by default)
[group('1. vira')]
run-vira ARGS='web --host 0.0.0.0 --base-path ${BASE_PATH:-/} --import ./sample.json':
    #!/usr/bin/env bash
    set -x
    # Workaround cabal/ghcid bug with $PATH mangling.
    export PATH=$(echo "$PATH" | tr ':' '\n' | grep '^/nix/store' | tr '\n' ':' | sed 's/:$//')
    # Vira now auto-generates TLS certificates as needed
    ghcid -T Main.main -c './cabal-repl vira:exe:vira' \
        --setup ":set args --state-dir ./state --auto-reset-state {{ ARGS }}"

# Run `vira ci` on itself.
ci:
    @just pc
    nix run . -- ci

# Run cabal tests (Pass, for example, `tail-test` to run for different component)
[group('2. haskell')]
test COMPONENT='vira-tests':
    ghcid --warnings -T Main.main -c "./cabal-repl {{ COMPONENT }}"

# Run ghcid, whilst writing output to ghcid.log (useful for LLM)
[group('2. haskell')]
ghcid COMPONENT='vira':
    ghcid --outputfile=ghcid.log -c "./cabal-repl {{ COMPONENT }}"

# Delete and recreate vira state during next `just run`
[group('1. vira')]
resetdb:
    # If this file is missing, Vira will reset state/job dirs.
    rm -f ./state/schema-version

[private]
pc_hooks:
    yq -r '.repos[].hooks[].name' .pre-commit-config.yaml | \
      grep -v hpack 
[private]
git_not_added:
    git status --porcelain | awk '{print $2}'

# Run pre-commit hooks on changed and untracked files
pc:
    # Run all but hpack, since that's slower
    for hook in $(just pc_hooks); do \
        pre-commit run $hook --files $(just git_not_added) ; \
    done
    # Then run hpack manually
    @just hpack

# Re-generate .cabal files
# This is faster than running pre-commit.
hpack:
    for f in $(find ./packages/ -name "package.yaml"); do \
        hpack $f; \
    done
