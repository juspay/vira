{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
    inputs.process-compose-flake.flakeModule
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
            pkgs.openssl # For automatic TLS certificate generation
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
      };

      # What should haskell-flake add to flake outputs?
      autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
    };

    process-compose."vira-dev" = {
      settings = {
        processes = {
          haskell = {
            command = pkgs.writeShellScriptBin "haskell-dev" ''
              set -x
              # Vira now auto-generates TLS certificates as needed
              ghcid -c 'cabal repl exe:vira --flags=ghcid' -T Main.main \
                  --setup ":set args --host 0.0.0.0 --base-path ''${BASE_PATH:-/}"
            '';
            depends_on.tailwind.condition = "process_started";
            # Without `SIGINT (2)` Vira doesn't close gracefully
            shutdown.signal = 2;
            readiness_probe = {
              exec.command = lib.getExe (pkgs.writeShellApplication {
                name = "vira-dev-health";
                runtimeInputs = [ pkgs.curl ];
                text = ''
                  curl -k https://0.0.0.0:5005
                '';
              });
            };
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
          setup = {
            command = pkgs.writeShellApplication {
              name = "vira-setup";
              runtimeInputs = [ pkgs.curl ];

              # The cachix token here is for a dummy cache, managed by Srid.
              text = ''
                curl -X POST -k \
                  --data-urlencode "name=vira" \
                  --data-urlencode "cloneUrl=https://github.com/juspay/vira.git" \
                  "https://0.0.0.0:5005/s/repo"
                curl -X POST -k \
                  --data-urlencode "cachixName=scratch-vira-dev" \
                  --data-urlencode "authToken=eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiI5NDI4ZjhkZi1mZWM5LTQ1ZjctYjMzYi01MTFiZTljNTNkNjciLCJzY29wZXMiOiJjYWNoZSJ9.WgPWUSYIie2rUdfuPqHS5mxrkT0lc7KIN7QPBPH4H-U" \
                  "https://0.0.0.0:5005/s/cachix"
              '';
            };
            depends_on.haskell.condition = "process_healthy";
          };
        };
      };
    };

    # Default package & app.
    packages.default = self'.packages.vira;
    apps.default = self'.apps.vira;
  };
}
