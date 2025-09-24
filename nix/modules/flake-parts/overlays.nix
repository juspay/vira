{ inputs, ... }:
{
  perSystem = { system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        (final: prev: {
          # https://github.com/cli/cli/pull/11544
          gh = prev.gh.overrideAttrs (oldAttrs: {
            src = inputs.gh-cli-pr-11544;
          });
        })
      ];
    };
  };
}
