# For running hpack watchers in development mode.
{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];
  perSystem = { self', pkgs, ... }: {
    haskellProjects.default.devShell.tools = _: {
      inherit (self'.packages) hpack-watch;
    };
    process-compose."hpack-watch" = {
      cli.options.no-server = true;
      settings = {
        processes = {
          hpack-configs = {
            command = pkgs.writeShellApplication {
              name = "hpack-watch-configs";
              runtimeInputs = [ pkgs.watchexec pkgs.just ];
              text = ''
                exec watchexec \
                  -f "**/package.yaml" \
                  -f "**/hpack-common.yaml" \
                  -f "**/hpack-relude.yaml" \
                  -f "**/cabal-repl" \
                  -- just hpack
              '';
            };
            is_tty = true;
          };
          hpack-hs-files = {
            command = pkgs.writeShellApplication {
              name = "hpack-watch-hs-files";
              runtimeInputs = [ pkgs.watchexec pkgs.just ];
              text = ''
                exec watchexec \
                  -f "**.hs" \
                  -e create \
                  -e remove \
                  -e rename \
                  -- just hpack
              '';
            };
            is_tty = true;
          };
        };
      };
    };
  };
}
