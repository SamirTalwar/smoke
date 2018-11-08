{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, ansi-terminal, base, data-default
      , Diff, directory, filepath, Glob, mtl, optparse-applicative
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
          aeson base data-default directory filepath Glob process
          process-extras text transformers vector yaml
        ];
        executableHaskellDepends = [
          aeson ansi-terminal base data-default Diff directory filepath Glob
          mtl optparse-applicative process process-extras temporary text
          transformers unix vector yaml
        ];
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
