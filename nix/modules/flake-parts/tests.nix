{ ... }:

{
  perSystem = { pkgs, lib, config, ... }: {
    # These tests require network access,
    # so run them in __noChroot (see below).
    haskellProjects.default.settings = {
      vira.check = false;
      git-effectful.check = false;
    };

    checks.tests = pkgs.runCommandNoCC "cabal-tests"
      {
        __noChroot = true;
        src = ../../../.;
        nativeBuildInputs = config.devShells.default.nativeBuildInputs ++ [ pkgs.cacert ];
        buildInputs = config.devShells.default.buildInputs;
      } ''
      export HOME=$TMPDIR

      # Set up SSL certificates for network access
      export SSL_CERT_FILE="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"

      # Set up VIRA environment variables using shared script
      source ${lib.getExe config.packages.vira-env}

      cp -r $src source
      cd source
      chmod -R u+w .

      cabal test all --test-show-details=direct

      # Capture test logs in output
      mkdir -p $out
      find . -name "*.log" -path "*/test/*" -exec cp {} $out/ \;
    '';
  };
}
