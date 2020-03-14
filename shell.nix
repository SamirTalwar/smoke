{ pkgs ? import <nixpkgs> {} }:

with pkgs;
mkShell {
  buildInputs = [
    git
    glibcLocales
    gmp
    nixpkgs-fmt
    openssl
    ormolu
    ruby
    stack
  ];
}
