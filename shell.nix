{ pkgs ? import <nixpkgs> {} }:

with pkgs;
mkShell {
  buildInputs = [
    stack
    nix
    git
    ruby
  ];
}
