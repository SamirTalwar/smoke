{ pkgs ? import ./nix/nixpkgs.nix {}
, ghc ? import ./nix/ghc.nix { inherit (pkgs) lib haskell; }
, drv ? import ./nix/smoke.nix { inherit ghc; inherit (pkgs) nix-gitignore; }
}:
let
  inherit (pkgs) haskell;
  inherit (haskell.lib) justStaticExecutables buildStrictly doStrip;
in
rec {
  smoke = justStaticExecutables (buildStrictly (doStrip drv));
  docker = import ./nix/docker.nix { inherit smoke; inherit (pkgs) dockerTools; };
}
