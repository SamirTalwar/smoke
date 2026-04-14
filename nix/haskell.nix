{ system ? builtins.currentSystem
, sources ? import ./nix/npins
, pkgs ? import sources.nixpkgs { inherit system; }
, ghcVersion ? pkgs.lib.strings.fileContents ../ghc.version
}:
let
  ghcName = "ghc" + pkgs.lib.strings.stringAsChars (c: if c == "." then "" else c) ghcVersion;
in
{
  lib = pkgs.haskell.lib;
  packages = pkgs.haskell.packages.${ghcName};
}
