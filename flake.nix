{
  description = "Smoke";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        sources = import ./nix/npins;
        pkgs = import sources.nixpkgs { inherit system; };
        haskell = import ./nix/haskell.nix { inherit system sources pkgs; };
        smoke = import ./. {
          inherit
            system
            sources
            pkgs
            haskell
            ;
        };
      in
      {
        packages.default = smoke;

        apps.default = flake-utils.lib.mkApp {
          drv = haskell.lib.overrideCabal (haskell.lib.justStaticExecutables (haskell.lib.doStrip smoke)) {
            enableSharedExecutables = false;
            enableSharedLibraries = false;
          };
        };

        formatter = pkgs.nixpkgs-fmt;

        devShells.default = import ./shell.nix {
          inherit
            system
            sources
            pkgs
            haskell
            ;
        };

        devShells.lint = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.packages.hlint
            haskell.packages.hpack
            haskell.packages.ormolu
          ];
          STACK_IN_NIX_SHELL = true;
        };
      }
    );
}
