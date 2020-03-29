#!/usr/bin/env nix-shell
#!nix-shell -I nixpkgs=./nixpkgs.nix -p ormolu nixpkgs-fmt haskellPackages.hlint -i bash

set -e

echo -e "* Checking nix formatting ...\n"
nixpkgs-fmt --check .

echo -e "* Checking haskell formatting ...\n"
ormolu --mode=check ./src

echo -e "* Linting haskell sources ...\n"
hlint ./src
