{ lib ? (import <nixpkgs> { }).lib }:
let
  config = { };
  overlays = [ ];
  version = lib.strings.fileContents ./nixpkgs.version;
  nixpkgs-unstable = import
    (
      builtins.fetchTarball {
        url = https://github.com/NixOS/nixpkgs-channels/archive/ + version + ".tar.gz";
      }
    );
in
nixpkgs-unstable { inherit config overlays; }
