{ pkgs ? import (import ./sources.nix).nixpkgs { }
, ghc ? import ./ghc.nix { inherit (pkgs) lib haskell; }
}:
let
in
with pkgs;
[
  ghc.haskell-language-server
  ghc.hlint
  git
  gnumake
  niv
  nixpkgs-fmt
  ormolu
  ruby
  stack
]
