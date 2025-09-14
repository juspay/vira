{ inputs, ... }:
{
  perSystem = { self', config, pkgs, ... }:
    let
      # Create a GHC environment with the packages we need for hint
      hintGhc = config.haskellProjects.default.outputs.finalPackages.ghcWithPackages (ps: with ps; [
        vira-types
        attic-hs
        git-effectful
      ]);
      # Environment variables required for `hint` to work correctly
      hintAttrs = rec {
        HINT_GHC_LIB_DIR = "${hintGhc}/lib/${hintGhc.meta.name}/lib";
        HINT_GHC_PACKAGE_PATH = "${HINT_GHC_LIB_DIR}/package.conf.d";
      };
    in
    {
      # Default shell.
      devShells.default = pkgs.mkShell {
        name = "vira-devshell";
        meta.description = "Haskell development environment";
        # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
        inputsFrom = [
          config.haskellProjects.default.outputs.devShell # See ./nix/modules/haskell.nix
          config.pre-commit.devShell # See ./nix/modules/formatter.nix
          config.devShells.jsAssets # See ./nix/modules/flake-parts/assets.nix
        ];
        packages = with pkgs; [
          just
          nixd
          yq

          # vira extraBuildDepends from haskell.nix
          # TODO: Can we find a way DRY this up?
          git
          cachix
          attic-client
          coreutils
          openssl
          gh-signoff
          self'.packages.nix
          self'.packages.omnix
        ];
        env = hintAttrs;
        shellHook = ''
          ${pkgs.lib.concatMapStringsSep "\n  " (attrs: "export ${attrs}=\"${hintAttrs.${attrs}}\"") (pkgs.lib.attrNames hintAttrs)}
          env | grep ^HINT_
        '';
      };

      haskellProjects.default = {
        # Add your package overrides here
        settings = {
          vira-repo-config = {
            drvAttrs = hintAttrs;
          };
        };
      };
    };
}
