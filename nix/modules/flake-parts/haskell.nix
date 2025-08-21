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

        # effectful
        co-log-effectful.source = inputs.co-log-effectful;
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
            pkgs.coreutils # For `tail`
            pkgs.omnix
            pkgs.openssl # For automatic TLS certificate generation
          ];
          stan = true;
        };
        warp-tls-simple = {
          extraBuildDepends = [
            pkgs.openssl # For automatic TLS certificate generation
          ];
        };
        servant-event-stream = {
          broken = false;
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
              # staticWhich) we still need them in PATH at runtime, so omnix
              # can access nix which then, transitively knows where to find
              # git.
              pkgs.git
              pkgs.nix
            ]}
        '';
      };
    };

    checks = config.haskellProjects.default.outputs.checks;
  };
}
