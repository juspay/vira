# Add external JavaScript assets to project.
{ inputs, ... }:
{
  perSystem = { pkgs, ... }:
    let
      # JavaScript assets derivation - reusable for both build and dev
      jsAssets = pkgs.runCommandNoCC "vira-js-assets" { } ''
        mkdir -p $out/js $out/js/htmx-extensions/src/sse
        cp ${pkgs.fetchurl {
          url = "https://unpkg.com/htmx.org@2.0.6/dist/htmx.min.js";
          sha256 = "sha256-tnaO7U86+Ftzp1BUcBvWDhfKxxiu8rf2slTl4OIEVhY=";
        }} $out/js/htmx.min.js
        cp ${pkgs.fetchurl {
          url = "https://unpkg.com/hyperscript.org@0.9.14/dist/_hyperscript.min.js";
          sha256 = "sha256-PoNKP/wDNP7lTs/043pq6VHNg+baqWZRynz9j3Ua1NI=";
        }} $out/js/hyperscript.min.js
        cp ${pkgs.fetchurl {
          url = "https://unpkg.com/htmx-ext-debug@2.0.1/debug.js";
          sha256 = "sha256-cWOu2G9ewcy+goB66NiU76aJJ+P1jhlXt3GmZGq+UDI=";
        }} $out/js/htmx-ext-debug.js
        cp ${inputs.htmx-extensions + /src/sse/sse.js} $out/js/htmx-extensions/src/sse/sse.js
      '';
    in
    {
      haskellProjects.default = {
        # Add it to Haskell Nix package
        settings = {
          vira = {
            custom = d: d.overrideAttrs (oa: {
              postUnpack = (oa.postUnpack or "") + ''
                ln -s ${jsAssets}/js $sourceRoot/static/js
              '';
            });
          };
        };

        # Add it to devShell (./static/js)
        devShell = {
          mkShellArgs.shellHook = ''
            # Set up JavaScript assets from Nix store using the same derivation as build
            rm -f ./static/js
            ln -sf ${jsAssets}/js ./static/js
          '';
        };
      };
    };
}
