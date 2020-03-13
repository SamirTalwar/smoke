{ pkgs ? import <nixpkgs> {} }:

with pkgs;
mkShell {
  buildInputs = [
    git
    nixpkgs-fmt
    ormolu
    ruby
    stack
  ];
}
