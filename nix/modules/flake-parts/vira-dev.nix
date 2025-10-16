# For running Vira in development mode. Called from `justfile`.
{ inputs, ... }:
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
                exec just run-vira "web --host ${host} --base-path ''${BASE_PATH:-/} --import ./sample.json"
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
    process-compose."vira-ghcid" = {
      settings = {
        processes = {
          ghcid = {
            command = pkgs.writeShellApplication {
              name = "ghcid-dev";
              text = ''
                exec just ghcid
              '';
            };
            is_tty = true;
          };
          hpack-watch =
            {
              command = pkgs.writeShellApplication {
                name = "hpack-watch-dev";
                runtimeInputs = [ pkgs.findutils pkgs.just ];
                text = ''
                  # Watch for changes to any package.yaml and re-run hpack
                  find ./packages -name "package.yaml" | ${lib.getExe pkgs.entr} -r ${lib.getExe pkgs.just} hpack
                '';
              };
              is_tty = true;
            };
        };
      };
    };
  };
}
