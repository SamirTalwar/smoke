{ pkgs
, ghc ? import ./ghc.nix { inherit (pkgs) lib haskell; }
}:
let
  smoke = ghc.callCabal2nix "smoke" (pkgs.nix-gitignore.gitignoreSource [ ] ../.) { };
  extraLibraries = import ./libs.nix { inherit pkgs; };
in
pkgs.haskell.lib.addExtraLibraries smoke extraLibraries
