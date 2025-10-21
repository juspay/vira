{ root, inputs, lib, ... }:
let
  devour-flake = lib.cleanSourceWith
    {
      name = "devour-flake";
      src = inputs.devour-flake;
    };
in
{
  imports = [
    inputs.haskell-flake.flakeModule
    (inputs.hint-nix + /flake-module.nix)
    (inputs.warp-tls-simple + /flake-module.nix)
  ];
  debug = true;
  perSystem = { self', inputs', lib, config, pkgs, ... }: {
    # Configure hint-nix with packages that vira needs
    hint-nix = {
      workaroundGhcPanic = true;
      packages = ps: with ps; [
        vira-ci-types
        git-effectful
      ];
    };
    haskellProjects.default = { config, ... }: {
      # To avoid unnecessary rebuilds, we filter projectRoot:
      # https://community.flake.parts/haskell-flake/local#rebuild
      projectRoot = lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /packages)
          (root + /cabal.project)
          (root + /LICENSE)
          (root + /README.md)
        ];
      };

      packages = {
        htmx.source = inputs.htmx + /htmx;
        htmx-lucid.source = inputs.htmx + /htmx-lucid;
        htmx-servant.source = inputs.htmx + /htmx-servant;

        # external libs
        co-log-effectful.source = inputs.co-log-effectful;
        tabler-icons.source = inputs.tabler-icons-hs;
        servant-event-stream.source = inputs.servant-event-stream;
        record-hasfield.source = inputs.record-hasfield;
        toml-reader.source = "0.3.0.0";
        filepattern.source = "0.1.3";
      };

      # Add your package overrides here
      settings = {
        vira = {
          imports = [
            config.settings.git-effectful
            config.settings.tail
            config.settings.warp-tls-simple
            config.settings.gh
            config.settings.attic
            config.settings.cachix
            config.settings.vira-ci-types
            config.settings.hint-nix
            config.settings.nix
            config.settings.devour-flake
          ];
          generateOptparseApplicativeCompletions = [ "vira" ];
          extraBuildDepends = [
            pkgs.attic-client # For attic
            pkgs.cachix # For cachix
          ];
          custom = drv: drv.overrideAttrs (oldAttrs: {
            postUnpack = (oldAttrs.postUnpack or "") + ''
              ln -s ${self'.packages.jsAssets}/js $sourceRoot/static/js

              # Replace "dev" with actual git hash in GitRev.hs
              gitRevFile="$sourceRoot/src/Vira/App/GitRev.hs"
              gitHash="${inputs.self.rev or "UNKNOWN"}"
              gitHashShort="''${gitHash:0:7}"
              ${pkgs.gnused}/bin/sed -i \
                -e "s/gitHashFull = \"dev\"/gitHashFull = \"$gitHash\"/" \
                -e "s/gitHashShort = \"dev\"/gitHashShort = \"$gitHashShort\"/" \
                "$gitRevFile"
            '';
            nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
            postInstall = (oldAttrs.postInstall or "") + ''
              # Required for building private repos, see https://github.com/juspay/vira/pull/166
              wrapProgram $out/bin/vira \
                --prefix PATH : ${lib.makeBinPath [ pkgs.openssh pkgs.git ]}
            '';
          });
        };
        git-effectful = {
          extraBuildDepends = [
            pkgs.git # For git
          ];
        };
        tail = {
          extraBuildDepends = [
            pkgs.coreutils # For `tail`
          ];
        };
        gh = {
          extraBuildDepends = [
            pkgs.gh-signoff
            pkgs.gh
          ];
        };
        attic = {
          extraBuildDepends = [
            pkgs.attic-client # For attic
          ];
        };
        cachix = {
          extraBuildDepends = [
            pkgs.cachix # For cachix
          ];
        };
        nix = {
          extraBuildDepends = [
            self'.packages.nix # For nix
          ];
        };
        devour-flake = {
          drvAttrs = {
            NIX_SYSTEMS_PATH = "${inputs.nix-systems}";
            DEVOUR_FLAKE_PATH = "${devour-flake}";
          };
        };
        safe-coloured-text-layout = {
          check = false;
          broken = false;
        };
        co-log-effectful.jailbreak = true;
        toml-reader.check = false;
      };

      # Development shell configuration
      devShell = {
        hlsCheck.enable = false;
        hoogle = false;
        tools = _: {
          stan = pkgs.haskellPackages.stan;
        } //
        # Bring all the `extraBuildDepends` (above) into the devShell, so
        # cabal/ghcid can resolve `staticWhich`.
        lib.flip lib.concatMapAttrs config.outputs.packages (_: v:
          lib.listToAttrs (lib.map (p: lib.nameValuePair p.name p) v.package.getCabalDeps.buildDepends)
        );
        mkShellArgs.shellHook = ''
          export NIX_SYSTEMS_PATH="${inputs.nix-systems}"
          export DEVOUR_FLAKE_PATH="${devour-flake}"
        '';
      };

      # What should haskell-flake add to flake outputs?
      autoWire = [ "checks" ];
    };


    # Explicitly define all outputs since autowiring is disabled
    packages = {
      default = config.haskellProjects.default.outputs.packages.vira.package;

      # The Nix version used by Vira
      # Nix 2.18 -> 2.22 are apprently buggy,
      # https://discourse.nixos.org/t/handling-git-submodules-in-flakes-from-nix-2-18-to-2-22-nar-hash-mismatch-issues/45118/5
      # So we use the latest.
      nix = pkgs.nixVersions.latest;
    };
  };
}
