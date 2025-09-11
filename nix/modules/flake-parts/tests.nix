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

      # Set up VIRA environment variables like in devShell
      export VIRA_GIT_BIN="${pkgs.lib.getExe' pkgs.git "git"}"
      export VIRA_ATTIC_BIN="${pkgs.lib.getExe' pkgs.attic-client "attic"}"
      export VIRA_CACHIX_BIN="${pkgs.lib.getExe' pkgs.cachix "cachix"}"
      export VIRA_OMNIX_BIN="${pkgs.lib.getExe' pkgs.omnix "om"}"
      export VIRA_OPENSSL_BIN="${pkgs.lib.getExe' pkgs.openssl "openssl"}"
      export VIRA_MKDIR_BIN="${pkgs.lib.getExe' pkgs.coreutils "mkdir"}"

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
