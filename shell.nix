{ pkgs ? import <nixpkgs> {} }:

with pkgs;
mkShell {
  buildInputs = [
    git
    gmp
    nixpkgs-fmt
    openssl
    ormolu
    ruby
    stack
  ];
}
