{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
    inputs.process-compose-flake.flakeModule
  ];
  perSystem = { self', lib, config, pkgs, ... }: {
    # Our only Haskell project. You can have multiple projects, but this template
    # has only one.
    # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
    haskellProjects.default = {
      # To avoid unnecessary rebuilds, we filter projectRoot:
      # https://community.flake.parts/haskell-flake/local#rebuild
      projectRoot = builtins.toString (lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /src)
          (root + /app)
          (root + /test)
          (root + /vira.cabal)
          (root + /LICENSE)
          (root + /README.md)
          (root + /.stan.toml)
        ];
      });

      # The base package set (this value is the default)
      # basePackages = pkgs.haskellPackages;

      # Packages to add on top of `basePackages`
      packages = {
        htmx.source = inputs.htmx + /htmx;
        htmx-lucid.source = inputs.htmx + /htmx-lucid;
        htmx-servant.source = inputs.htmx + /htmx-servant;

        # effectful
        effectful.source = inputs.effectful + /effectful;
        effectful-core.source = inputs.effectful + /effectful-core;
        effectful-plugin.source = inputs.effectful + /effectful-plugin;
        effectful-th.source = inputs.effectful + /effectful-th;
        th-abstraction.source = "0.6.0.0"; # New effectful-th requires it.
        co-log-effectful.source = inputs.co-log-effectful;

        # Syd's libs
        autodocodec.source = inputs.autodocodec + /autodocodec;
        autodocodec-nix.source = inputs.autodocodec + /autodocodec-nix;
        autodocodec-schema.source = inputs.autodocodec + /autodocodec-schema;
        autodocodec-yaml.source = inputs.autodocodec + /autodocodec-yaml;
        safe-coloured-text.source = inputs.safe-coloured-text + /safe-coloured-text;
        safe-coloured-text-layout.source = inputs.safe-coloured-text + /safe-coloured-text-layout;
        safe-coloured-text-gen.source = inputs.safe-coloured-text + /safe-coloured-text-gen;
        safe-coloured-text-layout-gen.source = inputs.safe-coloured-text + /safe-coloured-text-layout-gen;
        safe-coloured-text-terminfo.source = inputs.safe-coloured-text + /safe-coloured-text-terminfo;

      };

      # Add your package overrides here
      settings = {
        vira = {
          extraBuildDepends = [ pkgs.git ];
          stan = true;
        };

        safe-coloured-text-layout = {
          check = false;
          broken = false;
        };
        aeson.jailbreak = true; # Because we override th-abstraction
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
      autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
    };

    process-compose."vira-dev" = {
      settings = {
        processes = {
          haskell.command = "ghcid -c 'cabal repl exe:vira --flags=ghcid' -T :main";
          tailwind = {
            # Suppress tailwind output so only ghcid log comes through
            command = "echo Running tailwind silently.; ${lib.getExe pkgs.haskellPackages.tailwind} -w -o ./static/tailwind.css './src/**/*.hs' > tailwind.log 2>&1";
            is_tty = true;
          };
        };
      };
    };

    # Default package & app.
    packages.default = self'.packages.vira;
    apps.default = self'.apps.vira;
  };
}
