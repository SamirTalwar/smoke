{ pkgs ? import ./nixpkgs.nix {}
, ghc ? import ./ghc.nix { inherit (pkgs) lib haskell; }
, smoke ? import ./smoke.nix { inherit ghc; }
}:

ghc.shellFor {
  packages = _: [ smoke ];
  withHoogle = true;
  buildInputs = with pkgs; [
    cabal-install
    glibcLocales
    gmp
    ghc.hlint
    libiconv
    nix
    nixpkgs-fmt
    openssl
    ormolu
    zlib
  ];
}
