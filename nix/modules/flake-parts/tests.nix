{
  perSystem = { pkgs, config, ... }:
    let
      # Function to create a test check that runs a test executable directly
      createTestCheck = name: packageWithExecutable:
        pkgs.runCommandNoCC name
          {
            __noChroot = true; # Allow network access
            nativeBuildInputs = [ pkgs.cacert ];
          } ''
          export HOME=$TMPDIR

          # Set up SSL certificates for network access
          export SSL_CERT_FILE="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"

          # Run the test executable directly
          ${packageWithExecutable}/bin/${name} 2>&1 | tee $out
        '';
    in
    {

      # These tests require network access,
      # so run them in __noChroot (see below).
      haskellProjects.default.settings = {
        vira.check = false;
        git-effectful.check = false;
      };

      checks = {
        vira-tests = createTestCheck "vira-tests" config.haskellProjects.default.outputs.packages.vira.package;
        git-effectful-test = createTestCheck "git-effectful-test" config.haskellProjects.default.outputs.packages.git-effectful.package;
        gh-signoff-test = createTestCheck "gh-signoff-test" config.haskellProjects.default.outputs.packages.gh-signoff.package;
        tail-test = createTestCheck "tail-test" config.haskellProjects.default.outputs.packages.tail.package;
      };
    };
}
