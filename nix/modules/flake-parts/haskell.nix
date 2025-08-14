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

    process-compose."vira-dev" = {
      settings = {
        processes = let host = "0.0.0.0"; port = "5005"; in {
          haskell = {
            command = pkgs.writeShellScriptBin "haskell-dev" ''
              set -x
              # Vira now auto-generates TLS certificates as needed
              ghcid -c 'cabal repl exe:vira --flags=ghcid' -T Main.main \
                  --setup ":set args --host ${host} --base-path ''${BASE_PATH:-/}"
            '';
            depends_on.tailwind.condition = "process_started";
            # Without `SIGINT (2)` Vira doesn't close gracefully
            shutdown.signal = 2;
            readiness_probe = {
              exec.command = lib.getExe (pkgs.writeShellApplication {
                name = "vira-dev-health";
                runtimeInputs = [ pkgs.curl ];
                text = ''
                  curl -k https://${host}:${port}
                '';
                meta.description = "Checks if the vira server is running and accepting requests";
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
                  exec ${lib.getExe tailwind} -w -o ../packages/vira/static/tailwind.css --cwd ./packages/vira/src
                '';
              };
              is_tty = true;
            };
          setup = {
            command = pkgs.writeShellApplication {
              name = "vira-dev-setup";
              runtimeInputs = [ pkgs.curl ];

              text =
                let
                  defaultRepos = {
                    omnix = "https://github.com/juspay/omnix.git";
                    vira = "https://github.com/juspay/vira.git";
                    nixos-unified-template = "https://github.com/juspay/nixos-unified-template.git";
                    superposition = "https://github.com/juspay/superposition.git";
                  };
                in
                lib.concatMapStrings
                  (repo: ''
                    curl -X POST -k \
                      --data-urlencode "name=${repo.name}" \
                      --data-urlencode "cloneUrl=${repo.value}" \
                      "https://${host}:${port}/r/add"
                  '')
                  (lib.attrsToList defaultRepos) +
                # The cachix token here is for a dummy cache, managed by Srid.
                ''
                  curl -X POST -k \
                    --data-urlencode "cachixName=scratch-vira-dev" \
                    --data-urlencode "authToken=eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiI5NDI4ZjhkZi1mZWM5LTQ1ZjctYjMzYi01MTFiZTljNTNkNjciLCJzY29wZXMiOiJjYWNoZSJ9.WgPWUSYIie2rUdfuPqHS5mxrkT0lc7KIN7QPBPH4H-U" \
                    "https://${host}:${port}/settings/cachix"
                '';
              meta.description = ''
                A post-run setup script for Vira, that initialises a few repos and a dummy cachix.

                Note:
                  If the repos initialised by this script already exist in the state, the script will not override them.
                  The same is not true for the dummy cachix, it will be overriden.
              '';
            };
            depends_on.haskell.condition = "process_healthy";
          };
        };
      };
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

    apps.vira-tests = config.haskellProjects.default.outputs.apps.vira-tests;

    checks = config.haskellProjects.default.outputs.checks;
  };
}
