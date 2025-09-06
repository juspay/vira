{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  debug = true;
  perSystem = { self', lib, config, pkgs, ... }: {
    # Our only Haskell project. You can have multiple projects, but this template
    # has only one.
    # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
    haskellProjects.default = {
      # To avoid unnecessary rebuilds, we filter projectRoot:
      # https://community.flake.parts/haskell-flake/local#rebuild
      projectRoot = lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /packages)
          (root + /cabal.project)
          (root + /LICENSE)
          (root + /README.md)
          (root + /.stan.toml)
        ];
      };

      packages = {
        htmx.source = inputs.htmx + /htmx;
        htmx-lucid.source = inputs.htmx + /htmx-lucid;
        htmx-servant.source = inputs.htmx + /htmx-servant;

        # external libs
        co-log-effectful.source = inputs.co-log-effectful;
        tabler-icons.source = inputs.tabler-icons-hs;
        servant-event-stream.source = inputs.servant-event-stream;
      };

      # Add your package overrides here
      settings = {
        vira = {
          check = false; # Running outside of Nix.
          generateOptparseApplicativeCompletions = [ "vira" ];
          extraBuildDepends = [
            pkgs.git
            pkgs.cachix
            pkgs.attic-client
            pkgs.omnix
            pkgs.openssl # For automatic TLS certificate generation
          ];
          stan = true;
          custom = drv: drv.overrideAttrs (oldAttrs: {
            postUnpack = (oldAttrs.postUnpack or "") + ''
              ln -s ${self'.packages.jsAssets}/js $sourceRoot/static/js
            '';
            preBuild = (oldAttrs.preBuild or "") + ''
              export VIRA_GIT_BIN="${pkgs.lib.getExe' pkgs.git "git"}"
              export VIRA_ATTIC_BIN="${pkgs.lib.getExe' pkgs.attic-client "attic"}"
              export VIRA_CACHIX_BIN="${pkgs.lib.getExe' pkgs.cachix "cachix"}"
              export VIRA_OMNIX_BIN="${pkgs.lib.getExe' pkgs.omnix "om"}"
              export VIRA_OPENSSL_BIN="${pkgs.lib.getExe' pkgs.openssl "openssl"}"
            '';
          });
        };
        git-effectful = {
          check = false; # Running outside of Nix.
          custom = drv: drv.overrideAttrs (oldAttrs: {
            preBuild = (oldAttrs.preBuild or "") + ''
              export VIRA_GIT_BIN="${lib.getExe' pkgs.git "git"}"
            '';
          });
        };
        tail = {
          extraBuildDepends = [
            pkgs.coreutils # For `tail`
          ];
        };
        warp-tls-simple = {
          extraBuildDepends = [
            pkgs.openssl # For automatic TLS certificate generation
          ];
          custom = drv: drv.overrideAttrs (oldAttrs: {
            preBuild = (oldAttrs.preBuild or "") + ''
              export VIRA_OPENSSL_BIN="${lib.getExe' pkgs.openssl "openssl"}"
            '';
          });
        };
        safe-coloured-text-layout = {
          check = false;
          broken = false;
        };
        co-log-effectful.jailbreak = true;
      };

      # Development shell configuration
      devShell = {
        hlsCheck.enable = false;
        tools = _: {
          stan = pkgs.haskellPackages.stan;
          vira-dev = config.process-compose."vira-dev".outputs.package;
        };
      };

      # What should haskell-flake add to flake outputs?
      autoWire = [ ]; # Disable all autowiring
    };


    # Explicitly define all outputs since autowiring is disabled
    packages = {
      default = pkgs.symlinkJoin {
        name = "vira";
        paths = [ config.haskellProjects.default.outputs.packages.vira.package ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/vira \
            --prefix PATH : ${lib.makeBinPath [
              # Whilst git and nix are available at build time (for
              # staticWhichNix) we still need them in PATH at runtime, so omnix
              # can access nix which then, transitively knows where to find
              # git.
              pkgs.git
              self'.packages.nix
            ]}
        '';
      };

      # The Nix version used by Vira (thus omnix)
      # Nix 2.18 -> 2.22 are apprently buggy, 
      # https://discourse.nixos.org/t/handling-git-submodules-in-flakes-from-nix-2-18-to-2-22-nar-hash-mismatch-issues/45118/5
      # So we use the latest.
      nix = pkgs.nixVersions.latest;
    };

    checks = config.haskellProjects.default.outputs.checks;
  };
}
