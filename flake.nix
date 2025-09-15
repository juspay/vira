{
  nixConfig = {
    extra-substituters = "https://cache.nixos.asia/oss";
    extra-trusted-public-keys = "oss:KO872wNJkCDgmGN3xy9dT89WAhvv13EiKncTtHDItVU=";
  };
  description = "Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    nixos-unified.url = "github:srid/nixos-unified";
    haskell-flake.url = "github:srid/haskell-flake";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
    git-hooks.url = "github:bmrips/git-hooks.nix/typos-improvements"; # https://github.com/cachix/git-hooks.nix/pull/583
    git-hooks.flake = false;
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";

    htmx.url = "github:JonathanLorimer/htmx";
    htmx.flake = false;
    tabler-icons-hs.url = "github:juspay/tabler-icons-hs";
    tabler-icons-hs.flake = false;
    co-log-effectful.url = "github:eldritch-cookie/co-log-effectful";
    co-log-effectful.flake = false;
    # https://github.com/bflyblue/servant-event-stream/pull/13
    servant-event-stream.url = "github:bflyblue/servant-event-stream";
    servant-event-stream.flake = false;

    # Runtime dependencies
    htmx-extensions.url = "github:juspay/htmx-extensions/sse-unload"; # https://github.com/bigskysoftware/htmx-extensions/pull/147
    htmx-extensions.flake = false;
  };

  outputs = inputs:
    # https://nixos-unified.org/autowiring.html#flake-parts
    inputs.nixos-unified.lib.mkFlake
      { inherit inputs; root = ./.; };
}
