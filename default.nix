{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, ansi-terminal, base, cabal2nix
      , containers, data-default, Diff, directory, exceptions, filepath
      , Glob, hindent, hlint, hpack, mtl, optparse-applicative, path
      , process, process-extras, stdenv, temporary, text, transformers
      , unix, vector, yaml
      }:
      mkDerivation {
        pname = "smoke";
        version = "2.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base containers data-default directory exceptions filepath
          Glob path process process-extras text transformers vector yaml
        ];
        libraryToolDepends = [ cabal2nix hindent hlint hpack ];
        executableHaskellDepends = [
          aeson ansi-terminal base containers data-default Diff directory
          exceptions filepath Glob mtl optparse-applicative path process
          process-extras temporary text transformers unix vector yaml
        ];
        executableToolDepends = [ cabal2nix hindent hlint ];
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
