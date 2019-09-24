{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, ansi-terminal, base, cabal2nix
      , containers, data-default, Diff, directory, exceptions, filepath
      , Glob, hedgehog, hindent, hlint, hpack, hspec, hw-hspec-hedgehog
      , mtl, optparse-applicative, process, process-extras, stdenv, tar
      , temporary, text, transformers, unix, vector, yaml
      }:
      mkDerivation {
        pname = "smoke";
        version = "2.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base containers data-default directory exceptions filepath
          Glob process process-extras tar temporary text transformers vector
          yaml
        ];
        libraryToolDepends = [ cabal2nix hindent hlint hpack ];
        executableHaskellDepends = [
          aeson ansi-terminal base containers data-default Diff directory
          exceptions filepath Glob mtl optparse-applicative process
          process-extras tar temporary text transformers unix vector yaml
        ];
        executableToolDepends = [ cabal2nix hindent hlint ];
        testHaskellDepends = [
          aeson base containers data-default directory exceptions filepath
          Glob hedgehog hspec hw-hspec-hedgehog process process-extras tar
          temporary text transformers vector yaml
        ];
        testToolDepends = [ cabal2nix hindent hlint ];
        preConfigure = "hpack";
        homepage = "https://github.com/SamirTalwar/smoke#readme";
        description = "An integration test framework for console applications";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
