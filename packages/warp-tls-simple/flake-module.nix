{ lib, flake-parts-lib, ... }:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) mkOption types;
in
{
  options = {
    perSystem = mkPerSystemOption
      ({ config, options, pkgs, ... }: {
        options.warp-tls-simple = {
          enable = mkOption {
            type = types.bool;
            default = true;
            description = "Enable warp-tls-simple integration";
          };

          haskellProject = mkOption {
            type = types.str;
            default = "default";
            description = "Name of the haskell project to use for warp-tls-simple";
          };

          devShell = {
            enable = mkOption {
              type = types.bool;
              default = true;
              description = "Enable warp-tls-simple development shell with openssl";
            };

            name = mkOption {
              type = types.str;
              default = "warp-tls-simple";
              description = "Name of the warp-tls-simple development shell";
            };
          };
        };
      });
  };

  config = {
    perSystem = { config, pkgs, ... }:
      let
        cfg = config.warp-tls-simple;
      in
      lib.mkIf cfg.enable {
        # Development shell for warp-tls-simple
        devShells = lib.mkIf cfg.devShell.enable {
          ${cfg.devShell.name} = pkgs.mkShell {
            name = cfg.devShell.name;
            packages = with pkgs; [
              openssl
            ];
          };
        };

        # Configure the warp-tls-simple package in the haskell project
        haskellProjects.${cfg.haskellProject} = {
          settings = {
            warp-tls-simple = {
              extraBuildDepends = [
                pkgs.openssl # For openssl
              ];
            };
          };
        };
      };
  };
}
