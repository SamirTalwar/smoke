let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  ghc = import ./nix/ghc.nix { inherit (pkgs) lib haskell; };
  drv = import ./nix/smoke.nix { inherit ghc pkgs; };
  inherit (pkgs) haskell stdenv;
  inherit (haskell.lib) buildStrictly doStrip justStaticExecutables overrideCabal;
in
{
  smoke = overrideCabal (justStaticExecutables (buildStrictly (doStrip drv))) {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
  };
}
