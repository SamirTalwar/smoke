{ pkgs ? import <nixpkgs> {} }:
let
  version = pkgs.lib.strings.fileContents ./ghc.version;
  compiler = "ghc" + pkgs.lib.strings.stringAsChars (c: if c == "." then "" else c) version;
in
pkgs.haskell.packages.${compiler}
