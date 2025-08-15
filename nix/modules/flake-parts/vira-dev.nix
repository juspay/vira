# For running Vira in development mode. Called from `justfile`.
{ root, inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];
  perSystem = { self', lib, config, pkgs, ... }: {
    process-compose."vira-dev" = {
      settings = {
        processes = let host = "0.0.0.0"; port = "5005"; in {
          haskell = {
            command =
              let
                # https://x.com/sridca/status/1901283945779544362
                multiReplLibs = "vira:exe:vira vira tail warp-tls-simple htmx-lucid-contrib";
              in
              pkgs.writeShellScriptBin "haskell-dev" ''
                set -x
                cd ./packages/vira
                # Vira now auto-generates TLS certificates as needed
                ghcid -T Main.main -c 'cabal repl --enable-multi-repl ${multiReplLibs}' \
                    --setup ":set args --host ${host} --base-path ''${BASE_PATH:-/} --state-dir ../../state"
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
                  pwd
                  cd ./packages/vira
                  exec ${lib.getExe tailwind} -w -o ../static/tailwind.css --cwd ./src
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
  };
}
