{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
    inputs.process-compose-flake.flakeModule
  ];
  perSystem = { self', inputs', lib, config, pkgs, ... }: {
    # Our only Haskell project. You can have multiple projects, but this template
    # has only one.
    # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
    haskellProjects.default = {
      # To avoid unnecessary rebuilds, we filter projectRoot:
      # https://community.flake.parts/haskell-flake/local#rebuild
      projectRoot =
        let
          sourceHere = lib.fileset.toSource {
            inherit root;
            fileset = lib.fileset.unions [
              (root + /src)
              (root + /app)
              (root + /test)
              (root + /vira.cabal)
              (root + /LICENSE)
              (root + /README.md)
              (root + /.stan.toml)
              (root + /static)
            ];
          };
          sourceOutside = pkgs.writeTextDir "static/htmx-extensions/src/sse/sse.js" (builtins.readFile (inputs.htmx-extensions + /src/sse/sse.js));
          # Sadly, we can't use buildEnv or symlinkJoin here (build failure on Linux)
          combined = pkgs.runCommandNoCC "vira-src-combined" { } ''
            cp -r ${sourceHere} $out
            chmod -R u+w $out
            cp -r ${sourceOutside}/* $out/
          '';
        in
        combined;

      packages = {
        htmx.source = inputs.htmx + /htmx;
        htmx-lucid.source = inputs.htmx + /htmx-lucid;
        htmx-servant.source = inputs.htmx + /htmx-servant;

        # effectful
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
          extraBuildDepends = [
            pkgs.git
            pkgs.cachix
            pkgs.coreutils # For `tail`
            inputs'.omnix.packages.default
          ];
          stan = true;
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
        mkShellArgs.shellHook = ''
          ln -sf ${inputs.htmx-extensions} ./static/htmx-extensions
        '';
      };

      # What should haskell-flake add to flake outputs?
      autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
    };

    process-compose."vira-dev" = {
      settings = {
        processes = {
          # The cachix token here is for a dummy cache, managed by Srid.
          haskell = {
            command = ''
              set -x
              ghcid -c 'cabal repl exe:vira --flags=ghcid' -T Main.main \
                  --setup ':set args --repo-cachix-name scratch-vira-dev --repo-cachix-auth-token eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiI5NDI4ZjhkZi1mZWM5LTQ1ZjctYjMzYi01MTFiZTljNTNkNjciLCJzY29wZXMiOiJjYWNoZSJ9.WgPWUSYIie2rUdfuPqHS5mxrkT0lc7KIN7QPBPH4H-U'
            '';
            depends_on.tailwind.condition = "process_started";
          };
          tailwind =
            let
              tailwind = pkgs.tailwindcss_4;
            in
            {
              command = "${lib.getExe tailwind} -w -o ../static/tailwind.css --cwd ./src";
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
