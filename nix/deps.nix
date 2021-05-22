{ pkgs ? import (import ./sources.nix).nixpkgs { }
, ghc ? import ./ghc.nix { inherit (pkgs) lib haskell; }
}:
with pkgs;
[
  # Build
  gnumake
  stack

  # Test
  git
  ruby

  # Development
  coreutils
  dos2unix
  findutils
  ghc.haskell-language-server
  ghc.hlint
  gnused
  niv
  nixpkgs-fmt
  ormolu
]
