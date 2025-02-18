{
  description = "Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    nixos-unified.url = "github:srid/nixos-unified";
    haskell-flake.url = "github:srid/haskell-flake";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.flake = false;
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    tailwind-haskell.url = "github:srid/tailwind-haskell";
    tailwind-haskell.inputs.nixpkgs.follows = "nixpkgs";
    tailwind-haskell.inputs.flake-parts.follows = "flake-parts";
    tailwind-haskell.inputs.haskell-flake.follows = "haskell-flake";

    htmx.url = "github:JonathanLorimer/htmx";
    htmx.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    co-log-effectful.url = "github:eldritch-cookie/co-log-effectful";

    # Runtime dependencies
    omnix.url = "github:juspay/omnix";
  };

  outputs = inputs:
    # https://nixos-unified.org/autowiring.html#flake-parts
    inputs.nixos-unified.lib.mkFlake
      { inherit inputs; root = ./.; };
}
