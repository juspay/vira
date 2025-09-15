{
  perSystem = { config, pkgs, ... }:
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
      devShells.vira-repo-config = pkgs.mkShell {
        name = "vira-hint";
        env = hintAttrs;
        /* shellHook = ''
          ${pkgs.lib.concatMapStringsSep "\n  " (attrs: "export ${attrs}=\"${hintAttrs.${attrs}}\"") (pkgs.lib.attrNames hintAttrs)}
          env | grep ^HINT_
        '';
        */
      };

      haskellProjects.default = {
        settings = {
          vira-repo-config = {
            drvAttrs = hintAttrs;
          };
        };
      };
    };
}
