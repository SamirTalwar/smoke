{ pkgs ? import ./nix/nixpkgs.nix {}
, ghc ? import ./nix/ghc.nix { inherit (pkgs) lib haskell; }
, smoke ? import ./nix/smoke.nix { inherit ghc; inherit (pkgs) nix-gitignore; }
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
