{ root, ... }:
{
  flake.om = {
    ci = {
      default = {
        vira = {
          dir = ".";
          steps = {
            build = {
              enable = true;
            };
            # https://github.com/juspay/omnix/issues/486
            flake-check = {
              enable = false;
            };
          };
        };
        docs = {
          dir = "docs";
        };
        examples-nixos = {
          dir = "nix/examples/nixos";
          overrideInputs = {
            vira = root;
          };
        };
        examples-home-manager = {
          dir = "nix/examples/home-manager";
          overrideInputs = {
            vira = root;
          };
        };
      };
    };
  };
}
