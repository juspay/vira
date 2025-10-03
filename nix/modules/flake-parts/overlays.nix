{ inputs, ... }:
{
  perSystem = { system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        (final: prev: {
          # https://github.com/cli/cli/releases/tag/v2.81.0
          gh = prev.buildGoModule rec {
            pname = "gh";
            version = "2.81.0";

            src = inputs.gh-cli-v2-81-0;

            vendorHash = "sha256-rVNKTr3b4zShPfkiEBx7LqVQY2eMrXo/s8iC5tyQZNo=";

            nativeBuildInputs = [ prev.installShellFiles ];

            buildPhase = ''
              runHook preBuild
              make GO_LDFLAGS="-s -w -X github.com/cli/cli/v2/internal/build.Date=nixpkgs" GH_VERSION=${version} bin/gh ${prev.lib.optionalString (prev.stdenv.buildPlatform.canExecute prev.stdenv.hostPlatform) "manpages"}
              runHook postBuild
            '';

            installPhase =
              ''
                runHook preInstall
                install -Dm755 bin/gh -t $out/bin
              ''
              + prev.lib.optionalString (prev.stdenv.buildPlatform.canExecute prev.stdenv.hostPlatform) ''
                installManPage share/man/*/*.[1-9]

                installShellCompletion --cmd gh \
                  --bash <($out/bin/gh completion -s bash) \
                  --fish <($out/bin/gh completion -s fish) \
                  --zsh <($out/bin/gh completion -s zsh)
              ''
              + ''
                runHook postInstall
              '';

            doCheck = false;

            meta = {
              description = "GitHub CLI tool";
              homepage = "https://cli.github.com/";
              changelog = "https://github.com/cli/cli/releases/tag/v${version}";
              license = prev.lib.licenses.mit;
              mainProgram = "gh";
            };
          };
        })
      ];
    };
  };
}
