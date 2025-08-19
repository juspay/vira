{ inputs, ... }:
{
  imports = [
    (inputs.git-hooks + /flake-module.nix)
    inputs.fourmolu-nix.flakeModule
  ];
  perSystem = { config, ... }: {
    pre-commit.settings = {
      hooks = {
        nixpkgs-fmt.enable = true;

        fourmolu = {
          enable = true;
          package = config.fourmolu.wrapper;
        };
        hpack.enable = true;
        # FIXME: We use repo root `.hlint.yaml` which doesn't take per-package
        # settings (e.g.: no relude in `tail` package) into consideration.
        hlint.enable = true;

        typos = {
          enable = true;
          settings.config.files.extend-exclude = [
            "*.nix"
            ".hlint.yaml"
          ];
          settings.config.default.extend-identifiers = {
            TypeLits = "TypeLits"; # `GHC.TypeLits` module
          };
        };
      };
    };

    fourmolu.settings = {
      indentation = 2;
      comma-style = "leading";
      record-brace-space = true;
      indent-wheres = true;
      import-export-style = "diff-friendly";
      respectful = true;
      haddock-style = "multi-line";
      newlines-between-decls = 1;
      extensions = [ "ImportQualifiedPost" ];
    };
  };
}
