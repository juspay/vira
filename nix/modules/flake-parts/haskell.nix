{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
    inputs.process-compose-flake.flakeModule
  ];
  perSystem = { self', lib, config, pkgs, ... }:
    let
      # JavaScript assets derivation - reusable for both build and dev
      jsAssets = pkgs.runCommandNoCC "vira-js-assets" { } ''
        mkdir -p $out/js $out/js/htmx-extensions/src/sse
        cp ${pkgs.fetchurl {
          url = "https://unpkg.com/htmx.org@2.0.6/dist/htmx.min.js";
          sha256 = "sha256-tnaO7U86+Ftzp1BUcBvWDhfKxxiu8rf2slTl4OIEVhY=";
        }} $out/js/htmx.min.js
        cp ${pkgs.fetchurl {
          url = "https://unpkg.com/hyperscript.org@0.9.14/dist/_hyperscript.min.js";
          sha256 = "sha256-PoNKP/wDNP7lTs/043pq6VHNg+baqWZRynz9j3Ua1NI=";
        }} $out/js/hyperscript.min.js
        cp ${pkgs.fetchurl {
          url = "https://unpkg.com/htmx-ext-debug@2.0.1/debug.js";
          sha256 = "sha256-cWOu2G9ewcy+goB66NiU76aJJ+P1jhlXt3GmZGq+UDI=";
        }} $out/js/htmx-ext-debug.js
        cp ${inputs.htmx-extensions + /src/sse/sse.js} $out/js/htmx-extensions/src/sse/sse.js
      '';
    in
    {
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

            # Sadly, we can't use buildEnv or symlinkJoin here (build failure on Linux)
            combined = pkgs.runCommandNoCC "vira-src-combined" { } ''
              cp -r ${sourceHere} $out
              chmod -R u+w $out
              mkdir -p $out/static
              cp -r ${jsAssets}/* $out/static/
            '';
          in
          combined;

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
            generateOptparseApplicativeCompletions = [ "vira" ];
            extraBuildDepends = [
              pkgs.git
              pkgs.cachix
              pkgs.attic-client
              pkgs.coreutils # For `tail`
              pkgs.omnix
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
            # Set up JavaScript assets from Nix store using the same derivation as build
            rm -f ./static/js
            ln -sf ${jsAssets}/js ./static/js
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
              command = pkgs.writeShellScriptBin "haskell-dev" ''
                set -x
                ghcid -c 'cabal repl exe:vira --flags=ghcid' -T Main.main \
                    --setup ":set args --host 100.115.68.80 --cachix-name scratch-vira-dev --cachix-auth-token eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiI5NDI4ZjhkZi1mZWM5LTQ1ZjctYjMzYi01MTFiZTljNTNkNjciLCJzY29wZXMiOiJjYWNoZSJ9.WgPWUSYIie2rUdfuPqHS5mxrkT0lc7KIN7QPBPH4H-U --base-path ''${BASE_PATH:-/}"
              '';
              depends_on.tailwind.condition = "process_started";
              # Without `SIGINT (2)` Vira doesn't close gracefully
              shutdown.signal = 2;
            };
            tailwind =
              let
                tailwind = pkgs.tailwindcss_4;
              in
              {
                command = pkgs.writeShellApplication {
                  name = "tailwind-dev";
                  runtimeInputs = [ pkgs.watchman ];
                  text = ''
                    exec ${lib.getExe tailwind} -w -o ../static/tailwind.css --cwd ./src
                  '';
                };
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
