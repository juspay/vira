{ inputs, ... }:
{
  perSystem = { system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        (final: prev: {
          # https://github.com/cli/cli/releases/tag/v2.81.0
          gh = prev.gh.overrideAttrs (oldAttrs: {
            src = inputs.gh-cli-v2-81-0;
          });
        })
      ];
    };
  };
}
