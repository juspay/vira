{
  perSystem = { pkgs, config, ... }:
    let
      # Like runCommand, but with network access enabled
      # Requires `sandbox = relaxed` in CI's nix.conf
      runCommandWithInternet = name: script:
        pkgs.runCommand name
          {
            __noChroot = true; # Allow network access
            nativeBuildInputs = [ pkgs.cacert ];
          } ''
          # Set up SSL certificates for network access
          export SSL_CERT_FILE="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"

          ${script}
        '';
      # Instead of `cabal test` (in Nix devShell), run the test executable directly
      createTestCheck = name: p:
        runCommandWithInternet name ''
          # Run the test executable directly
          ${p}/bin/${name} 2>&1 | tee $out
        '';
    in
    {

      # These tests require network access,
      # so run them in __noChroot (see below).
      haskellProjects.default.settings = {
        vira.check = false;
        git-effectful.check = false;
      };

      checks =
        let
          hsPkgs = config.haskellProjects.default.outputs.packages;
        in
        {
          vira-tests = createTestCheck "vira-tests" hsPkgs.vira.package;
          git-effectful-test = createTestCheck "git-effectful-test" hsPkgs.git-effectful.package;
          gh-test = createTestCheck "gh-test" hsPkgs.gh.package;
        };
    };
}
