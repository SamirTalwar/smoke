{ pkgs ? import <nixpkgs> {}, ghc ? import ./ghc.nix { pkgs = pkgs; } }:

with pkgs;
let
  app = ghc.callPackage ./app.nix {};
in
mkShell {
  buildInputs = app.env.buildInputs ++ [
    git
    ruby
  ];
  nativeBuildInputs = app.env.nativeBuildInputs ++ [
    cabal-install
    cabal2nix
    glibcLocales
    gmp
    libiconv
    nix
    nixpkgs-fmt
    openssl
    ormolu
    stack
    zlib
  ];
}
