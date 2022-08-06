{
  description = "Smoke";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;

    nixpkgs.url = github:NixOS/nixpkgs/master;
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    let
      name = "smoke";
    in
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      deps = import ./nix/deps.nix { inherit pkgs; };
      ghc = import ./nix/ghc.nix {
        inherit (pkgs) lib haskell;
        ghcVersion = pkgs.lib.strings.fileContents ./ghc.version;
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
        buildInputs = deps;
      };
    }
    );
}
