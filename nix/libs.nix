{ pkgs }:
let
  gmp6 = pkgs.gmp6.override { withStatic = true; };
  libffi = pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; });
  libiconv = pkgs.libiconv.overrideAttrs (old: { enableStatic = true; enableShared = false; });
  openssl = pkgs.openssl.override { static = true; };
  zlib = pkgs.zlib.static;
in
[
  gmp6
  libiconv
  openssl
  zlib
]
