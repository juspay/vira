{ lib, flake-parts-lib, ... }:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) mkOption types;
in
{
  options = {
    perSystem = mkPerSystemOption
      ({ config, options, pkgs, ... }: {
        options.hint-nix = {
          enable = mkOption {
            type = types.bool;
            default = true;
            description = "Enable hint-nix integration";
          };

          packages = mkOption {
            type = types.functionTo (types.listOf types.package);
            default = ps: [ ];
            description = ''
              Function that takes package set and returns list of Haskell packages
              to make available in the hint interpreter environment.
              Example: `ps: with ps; [ my-package another-package ]`
            '';
          };

          haskellProject = mkOption {
            type = types.str;
            default = "default";
            description = "Name of the haskell project to use for hint-nix";
          };

          devShell = {
            enable = mkOption {
              type = types.bool;
              default = true;
              description = "Enable hint-nix development shell";
            };

            name = mkOption {
              type = types.str;
              default = "hint-nix";
              description = "Name of the hint-nix development shell";
            };
          };

          workaroundGhcPanic = mkOption {
            type = types.bool;
            default = true;
            description = "Enable workaround for GHC panic on macOS (Relocation target for PAGE21 out of range)";
          };
        };
      });
  };

  config = {
    perSystem = { config, pkgs, ... }:
      let
        cfg = config.hint-nix;
        haskellProjectCfg = config.haskellProjects.${cfg.haskellProject};

        # Create a GHC environment with the user-specified packages
        hintGhc = haskellProjectCfg.outputs.finalPackages.ghcWithPackages cfg.packages;

        # Environment variables required for `hint` to work correctly
        hintAttrs = rec {
          HINT_GHC_LIB_DIR = "${hintGhc}/lib/${hintGhc.meta.name}/lib";
          HINT_GHC_PACKAGE_PATH = "${HINT_GHC_LIB_DIR}/package.conf.d";
        };
      in
      lib.mkIf cfg.enable {
        # Development shell for hint-nix
        devShells = lib.mkIf cfg.devShell.enable {
          ${cfg.devShell.name} = pkgs.mkShell {
            name = cfg.devShell.name;
            shellHook = ''
              ${lib.concatMapStringsSep "\n  " (attrs: "export ${attrs}=\"${hintAttrs.${attrs}}\"") (lib.attrNames hintAttrs)}
              env | grep ^HINT_
            '';
          };
        };

        # Configure the hint-nix package in the haskell project
        haskellProjects.${cfg.haskellProject} = {
          settings = {
            hint-nix = {
              drvAttrs = hintAttrs;

              # Fix GHC panic on macOS: `Relocation target for PAGE21 out of range.`
              # This happens on some, if not all, uses of `hint`.
              sharedLibraries = cfg.workaroundGhcPanic;
              sharedExecutables = cfg.workaroundGhcPanic;
            };
          };
        };
      };
  };
}

