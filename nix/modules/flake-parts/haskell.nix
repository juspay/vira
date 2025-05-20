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
      };

      # Add your package overrides here
      settings = {
        vira = {
          extraBuildDepends = [
            pkgs.git
            pkgs.cachix
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
          rm -f ./static/htmx-extensions
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
            command = pkgs.writeShellScriptBin "haskell-dev" ''
              set -x
              ghcid -c 'cabal repl exe:vira --flags=ghcid' -T Main.main \
                  --setup ":set args --cachix-name scratch-vira-dev --cachix-auth-token eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiI5NDI4ZjhkZi1mZWM5LTQ1ZjctYjMzYi01MTFiZTljNTNkNjciLCJzY29wZXMiOiJjYWNoZSJ9.WgPWUSYIie2rUdfuPqHS5mxrkT0lc7KIN7QPBPH4H-U --base-path ''${BASE_PATH:-/}"
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
