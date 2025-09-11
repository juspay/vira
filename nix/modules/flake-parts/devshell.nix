{
  perSystem = { self', config, pkgs, ... }: {
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
        self'.packages.nix
        self'.packages.omnix
      ];
    };
  };
}
