{ root, ... }:

{
  perSystem = { pkgs, lib, config, ... }:
    let
      # Create a flake cheeck that runs the command in the given devShel
      flakeCheckInDevShell = devShell: name: shellAttrFilter: text:
        pkgs.runCommandNoCC name
          ({
            __noChroot = true;
            src = root;
            nativeBuildInputs = [ pkgs.cacert ] ++ devShell.nativeBuildInputs;
            buildInputs = devShell.buildInputs;
          } // lib.filterAttrs shellAttrFilter devShell) ''
          export HOME=$TMPDIR

          # Set up SSL certificates for network access
          export SSL_CERT_FILE="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"

          cp -r $src source
          cd source
          chmod -R u+w .

          ${text}
        '';
    in
    {

      # These tests require network access,
      # so run them in __noChroot (see below).
      haskellProjects.default.settings = {
        vira.check = false;
        git-effectful.check = false;
      };

      checks.tests = flakeCheckInDevShell config.devShells.default "cabal-test" (name: _: lib.hasPrefix "VIRA_" name) ''
        cabal test all --test-show-details=direct

        # Capture test logs in output
        mkdir -p $out
        find . -name "*.log" -path "*/test/*" -exec cp {} $out/ \;
      '';
    };
}
