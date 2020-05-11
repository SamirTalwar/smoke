{ pkgs ? import ./nix/nixpkgs.nix { }
, ghc ? import ./nix/ghc.nix { inherit (pkgs) lib haskell; }
, drv ? import ./nix/smoke.nix { inherit ghc pkgs; }

}:
let
  inherit (pkgs) haskell stdenv;
  inherit (haskell.lib) buildStrictly doStrip justStaticExecutables overrideCabal;
in
{
  smoke = overrideCabal (justStaticExecutables (buildStrictly (doStrip drv))) {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
  };
}
