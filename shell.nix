{ pkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:

with pkgs;
let
  app = pkgs.haskell.packages.${compiler}.callPackage ./app.nix {};
in
mkShell {
  buildInputs = app.env.buildInputs ++ [
    git
    ruby
  ];
  nativeBuildInputs = app.env.nativeBuildInputs ++ [
    cabal2nix
    glibcLocales
    nix
    nixpkgs-fmt
    ormolu
    stack
  ];
}
