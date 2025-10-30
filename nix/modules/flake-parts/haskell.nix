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
  perSystem = { self', lib, config, pkgs, ... }: {
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
        aeson-casing.source = "0.2.0.0";
        neat-interpolation.source = "0.5.1.4";

        # nix-serve-ng as library
        nix-serve-ng.source =
          let
            transformedSource = pkgs.runCommand "nix-serve-ng-transformed" { } ''
                  cp -r ${inputs.nix-serve-ng} $out
                  chmod -R u+w $out

                  # Create NixServeNg directory
                  mkdir -p $out/src/NixServeNg

                  # Extract makeApplication to Application.hs (lines 1-319 from Main.hs)
                  ${pkgs.gnused}/bin/sed -n '1,319p' $out/src/Main.hs > $out/src/NixServeNg/Application.hs

                  # Update Application.hs: change module name
                  ${pkgs.gnused}/bin/sed -i 's/^module Main where$/module NixServeNg.Application (\n  ApplicationOptions (..),\n  makeApplication,\n  validHashPart,\n  validHashPartBytes,\n) where/' $out/src/NixServeNg/Application.hs

                  # Remove CLI-specific imports
                  ${pkgs.gnused}/bin/sed -i '/^import Options/d' $out/src/NixServeNg/Application.hs
                  ${pkgs.gnused}/bin/sed -i '/^import qualified Options/d' $out/src/NixServeNg/Application.hs
                  ${pkgs.gnused}/bin/sed -i '/^import qualified System\.Environment/d' $out/src/NixServeNg/Application.hs
                  ${pkgs.gnused}/bin/sed -i '/^import qualified Network\.Socket/d' $out/src/NixServeNg/Application.hs
                  ${pkgs.gnused}/bin/sed -i '/^import qualified Network\.Wai\.Handler\.Warp/d' $out/src/NixServeNg/Application.hs
                  ${pkgs.gnused}/bin/sed -i '/^import qualified Network\.Wai\.Handler\.WarpTLS/d' $out/src/NixServeNg/Application.hs
                  ${pkgs.gnused}/bin/sed -i '/^import qualified Network\.Wai\.Middleware\.RequestLogger/d' $out/src/NixServeNg/Application.hs
                  ${pkgs.gnused}/bin/sed -i '/^import Network\.Socket/d' $out/src/NixServeNg/Application.hs
                  ${pkgs.gnused}/bin/sed -i '/^import Data\.Function/d' $out/src/NixServeNg/Application.hs
                  ${pkgs.gnused}/bin/sed -i '/^import Numeric\.Natural/d' $out/src/NixServeNg/Application.hs

                  # Delete CLI-specific files
                  rm -f $out/src/Options.hs $out/src/Main.hs

                  # Create NixServeNg.hs wrapper
                  cat > $out/src/NixServeNg.hs << 'EOF'
              {-# LANGUAGE OverloadedStrings #-}

              module NixServeNg (
                -- * Application
                makeApplication,
                ApplicationOptions (..),

                -- * Re-exports from Nix module
                Nix.initStore,
                Nix.getStoreDir,
                Nix.PathInfo (..),
                Nix.NoSuchPath (..),
              ) where

              import NixServeNg.Application (ApplicationOptions (..), makeApplication)
              import Nix qualified
              EOF

                  # Patch cbits/nix.cpp for newer Nix
                  ${pkgs.gnused}/bin/sed -i 's|#include <nix/store/store-api.hh>|#include <nix/store/store-api.hh>\n    #include <nix/store/store-open.hh>|' $out/cbits/nix.cpp
                  ${pkgs.gnused}/bin/sed -i 's|#include <nix/store/log-store.hh>|#include <nix/store/log-store.hh>\n    #include <nix/store/globals.hh>|' $out/cbits/nix.cpp

                  # Transform cabal file from executable to library
                  ${pkgs.gnused}/bin/sed -i 's/^executable nix-serve$/library/' $out/nix-serve-ng.cabal
                  ${pkgs.gnused}/bin/sed -i 's/main-is: *Main\.hs/exposed-modules:  NixServeNg\n                    , NixServeNg.Application\n                    , Nix/' $out/nix-serve-ng.cabal

                  # Remove other-modules (it's multiline: Nix, Options)
                  ${pkgs.gnused}/bin/sed -i '/other-modules:/,/Options/d' $out/nix-serve-ng.cabal

                  ${pkgs.gnused}/bin/sed -i 's/, optparse-applicative//' $out/nix-serve-ng.cabal
                  ${pkgs.gnused}/bin/sed -i 's/cxx-options: *-std=c++20/cxx-options:      -std=c++23/' $out/nix-serve-ng.cabal
                  ${pkgs.gnused}/bin/sed -i 's/ghc-options: *-Wall -threaded -O2 -rtsopts/ghc-options:      -Wall -O2/' $out/nix-serve-ng.cabal

                  # Add ImportQualifiedPost after hs-source-dirs (inside library section)
                  ${pkgs.gnused}/bin/sed -i '/hs-source-dirs:/a\    default-extensions: ImportQualifiedPost' $out/nix-serve-ng.cabal

                  # Remove benchmark section
                  ${pkgs.gnused}/bin/sed -i '/^benchmark benchmark/,$d' $out/nix-serve-ng.cabal
            '';
          in
          transformedSource;
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
            pkgs.git-lfs
            pkgs.openssh # `ssh` cli, for cloning private repos
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
        nix-serve-ng = {
          check = false;
          custom = drv: pkgs.haskell.lib.overrideCabal drv (old: {
            libraryHaskellDepends = (old.libraryHaskellDepends or [ ]) ++ (with pkgs.haskellPackages; [
              base16
              base32
              charset
              managed
              http-types
              megaparsec
              network
              vector
              wai
              wai-extra
              warp
              warp-tls
            ]);
            librarySystemDepends = (old.librarySystemDepends or [ ]) ++ [
              pkgs.boost.dev
              pkgs.nixVersions.latest
            ];
            libraryToolDepends = (old.libraryToolDepends or [ ]) ++ [ pkgs.pkg-config ];
            libraryPkgconfigDepends = (old.libraryPkgconfigDepends or [ ]) ++ [
              pkgs.libblake3
              pkgs.openssl
              pkgs.libsodium
              pkgs.sqlite
              pkgs.brotli
              pkgs.curl
            ];
          });
        };
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
