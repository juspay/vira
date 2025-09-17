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
      devShells.hint-nix = pkgs.mkShell {
        name = "vira-hint";
        shellHook = ''
          ${pkgs.lib.concatMapStringsSep "\n  " (attrs: "export ${attrs}=\"${hintAttrs.${attrs}}\"") (pkgs.lib.attrNames hintAttrs)}
          env | grep ^HINT_
        '';
      };

      haskellProjects.default = {
        settings = {
          hint-nix = {
            drvAttrs = hintAttrs;

            # Fix GHC panic on macOS: `Relocation target for PAGE21 out of range.`
            # This happens on some, if not all, uses of `hint`.
            sharedLibraries = true;
            sharedExecutables = true;
          };
        };
      };
    };
}
