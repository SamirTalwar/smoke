{ ghc, pkgs }:
let
  smoke = ghc.callCabal2nix "smoke" (pkgs.nix-gitignore.gitignoreSource [] ../.) {};
  extraLibraries = with pkgs; [
    gmp
    libiconv
    openssl
    zlib
  ];
in
pkgs.haskell.lib.addExtraLibraries smoke extraLibraries
