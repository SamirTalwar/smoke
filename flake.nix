{
  description = "Smoke";

  inputs = {
    flake-utils = {
      url = github:numtide/flake-utils;
    };

    nixpkgs = {
      url = github:NixOS/nixpkgs/master;
    };

    haskellTar = {
      url = github:haskell/tar/dbf8c995153c8a80450724d9f94cf33403740c80;
      flake = false;
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , haskellTar
    , flake-compat
    }:
    let
      name = "smoke";
    in
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      deps = import ./nix/deps.nix { inherit pkgs; };
      extraLibs = import ./nix/libs.nix { inherit pkgs; };
      ghc = import ./nix/ghc.nix {
        inherit (pkgs) lib haskell;
        ghcVersion = pkgs.lib.strings.fileContents ./ghc.version;
        overrides = hself: hsuper: {
          tar = hsuper.callCabal2nixWithOptions "tar" haskellTar "--no-check" { };
        };
      };
      drv = import ./nix/smoke.nix { inherit ghc pkgs; };
    in
    rec {
      packages.default = drv;
      apps.default = flake-utils.lib.mkApp {
        drv = pkgs.haskell.lib.overrideCabal (pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.lib.doStrip drv)) {
          enableSharedExecutables = false;
          enableSharedLibraries = false;
        };
      };

      formatter = pkgs.nixpkgs-fmt;

      devShells.default = pkgs.mkShell {
        buildInputs = deps ++ [ ghc.ghc ] ++ extraLibs;

        # Necessary until https://github.com/commercialhaskell/stack/issues/5008 is fixed.
        STACK_IN_NIX_SHELL = true;
      };
    }
    );
}
