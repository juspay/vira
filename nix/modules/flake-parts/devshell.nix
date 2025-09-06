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
      shellHook = ''
        export VIRA_GIT_BIN="${pkgs.lib.getExe' pkgs.git "git"}"
        export VIRA_ATTIC_BIN="${pkgs.lib.getExe' pkgs.attic-client "attic"}"
        export VIRA_CACHIX_BIN="${pkgs.lib.getExe' pkgs.cachix "cachix"}"
        export VIRA_OMNIX_BIN="${pkgs.lib.getExe' pkgs.omnix "om"}"
        export VIRA_OPENSSL_BIN="${pkgs.lib.getExe' pkgs.openssl "openssl"}"
      '';
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
