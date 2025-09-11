{ ... }:

{
  perSystem = { pkgs, config, ... }: {
    # Run `cabal test all` as part of building this derivation.
    # Since some tests require network access (git-effectful), we set __noChroot.
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
      source ${config.packages.vira-env}/bin/vira-env

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
