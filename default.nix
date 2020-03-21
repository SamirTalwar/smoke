{ pkgs ? import <nixpkgs> {}, ghc ? import ./ghc.nix { pkgs = pkgs; } }:

ghc.callPackage ./app.nix {}
