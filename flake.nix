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
    git-hooks.url = "github:bmrips/git-hooks.nix";
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
    servant-github-webhook.url = "github:tsani/servant-github-webhook/pull/18/head";
    servant-github-webhook.flake = false;
    hint-nix.url = "github:srid/hint-nix";
    hint-nix.flake = false;
    warp-tls-simple.url = "github:srid/warp-tls-simple";
    warp-tls-simple.flake = false;
    record-hasfield.url = "github:ndmitchell/record-hasfield";
    record-hasfield.flake = false;
    nix-systems.url = "github:srid/nix-systems";
    devour-flake.url = "github:srid/devour-flake";
    devour-flake.flake = false;
    nix-serve-ng.url = "github:aristanetworks/nix-serve-ng";
    nix-serve-ng.flake = false;

    # Runtime dependencies
    htmx-extensions.url = "github:juspay/htmx-extensions/sse-unload"; # https://github.com/bigskysoftware/htmx-extensions/pull/147
    htmx-extensions.flake = false;
  };

  outputs = inputs:
    # https://nixos-unified.org/autowiring.html#flake-parts
    inputs.nixos-unified.lib.mkFlake
      { inherit inputs; root = ./.; };
}
