{ pkgs ? import <nixpkgs> {} }:

with pkgs;
mkShell {
  buildInputs = [
    cabal2nix
    git
    glibcLocales
    gmp
    nix
    nixpkgs-fmt
    openssl
    ormolu
    ruby
    stack
  ];
}
