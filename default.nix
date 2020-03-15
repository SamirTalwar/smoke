{ pkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:

pkgs.haskell.packages.${compiler}.callPackage ./app.nix {}
