{ inputs, pkgs }:

# Create a transformed source first
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

hfinal: hprev: {
  nix-serve-ng = pkgs.haskell.lib.overrideCabal
    (hprev.callCabal2nix "nix-serve-ng" transformedSource { })
    (old: {
      doCheck = false; # Disable tests

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
}
