{ pkgs ? import ./nix/nixpkgs.nix { }
, ghc ? import ./nix/ghc.nix { inherit (pkgs) lib haskell; }
, drv ? import ./nix/smoke.nix { inherit ghc pkgs; }
}:

ghc.shellFor {
  packages = _: [ drv ];
  withHoogle = true;
  buildInputs = with pkgs; [
    cabal-install
    ghc.hlint
    git
    nixpkgs-fmt
    ormolu
    ruby
  ];
}
