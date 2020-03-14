{ pkgs ? import <nixpkgs> {} }:

with pkgs;
mkShell {
  buildInputs = [
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
