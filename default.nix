{ pkgs ? import ./nix/nixpkgs.nix {}
, ghc ? import ./nix/ghc.nix { inherit (pkgs) lib haskell; }
, drv ? import ./nix/smoke.nix { inherit ghc pkgs; }
}:
let
  inherit (pkgs) haskell;
  inherit (haskell.lib) justStaticExecutables buildStrictly doStrip;
in
{
  smoke = justStaticExecutables (buildStrictly (doStrip drv));
}
