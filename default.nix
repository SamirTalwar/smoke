{ pkgs ? import ./nixpkgs.nix {}
, ghc ? import ./ghc.nix { inherit (pkgs) lib haskell; }
, drv ? import ./smoke.nix { inherit ghc; inherit (pkgs) nix-gitignore; }
}:
let
  inherit (pkgs) haskell;
  inherit (haskell.lib) justStaticExecutables buildStrictly doStrip;
in
rec {
  smoke = justStaticExecutables (buildStrictly (doStrip drv));
  docker = import ./docker.nix { inherit smoke; inherit (pkgs) dockerTools; };
}
