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
        ghciwatch
        yq
        # vira extraBuildDepends from haskell.nix
        git
        cachix
        attic-client
        coreutils # For `tail`
        omnix
        openssl # For TLS certificate generation
        self'.packages.nix
      ];
    };
  };
}
