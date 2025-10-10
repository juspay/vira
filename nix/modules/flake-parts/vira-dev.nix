# For running Vira in development mode. Called from `justfile`.
{ root, inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];
  perSystem = { self', lib, pkgs, ... }: {
    haskellProjects.default.devShell.tools = _: {
      inherit (self'.packages) vira-dev;
    };
    process-compose."vira-dev" = {
      settings = {
        processes = let host = "0.0.0.0"; port = "5005"; in {
          haskell = {
            command =
              pkgs.writeShellScriptBin "haskell-dev" ''
                set -x
                # Workaround cabal/ghcid bug with $PATH mangling.
                export PATH=$(echo "$PATH" | tr ':' '\n' | grep '^/nix/store' | tr '\n' ':' | sed 's/:$//')
                # Vira now auto-generates TLS certificates as needed
                ghcid -T Main.main -c '${root}/cabal-repl vira:exe:vira' \
                    --setup ":set args --state-dir ./state --auto-reset-state web --host ${host} --base-path ''${BASE_PATH:-/} --import ./sample.json"
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
        };
      };
    };
  };
}
