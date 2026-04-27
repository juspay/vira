# /do config

## Check command

nix develop -c cabal build all

## Format command

just pc

## Test command

cabal test all

## CI command

nix run . -- ci

## Documentation

Keep `README.md` in sync with user-facing changes.

<!-- Optional (add manually for the evidence step):
## PR evidence
-->
