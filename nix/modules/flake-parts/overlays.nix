{ inputs, ... }:
{
  perSystem = { system, ... }: {
    # Configure overlays for nixpkgs
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        (final: prev: {
          # Mark nodejs as broken on darwin to prevent accidental usage
          # Use nodejs_24 instead for darwin compatibility
          nodejs =
            if prev.stdenv.isDarwin
            then prev.nodejs.overrideAttrs (old: { meta = old.meta // { broken = true; }; })
            else prev.nodejs;
        })
      ];
    };
  };
}
