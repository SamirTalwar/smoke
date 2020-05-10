{ ghc, pkgs }:
let
  smoke = ghc.callCabal2nix "smoke" (pkgs.nix-gitignore.gitignoreSource [ ] ../.) { };
  gmp6 = pkgs.gmp6.override { withStatic = true; };
  libffi = pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; });
  libiconv = pkgs.libiconv.overrideAttrs (old: { enableStatic = true; enableShared = false; });
  openssl = pkgs.openssl.override { static = true; };
  zlib = pkgs.zlib.static;
  extraLibraries = [
    gmp6
    libiconv
    openssl
    zlib
  ];
in
pkgs.haskell.lib.addExtraLibraries smoke extraLibraries
