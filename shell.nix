let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  ghc = import ./nix/ghc.nix { inherit (pkgs) lib haskell; };
  drv = import ./nix/smoke.nix { inherit ghc pkgs; };
in
ghc.shellFor {
  packages = _: [ drv ];
  withHoogle = true;
  buildInputs = with pkgs; [
    cabal-install
    ghc.hlint
    git
    niv
    nixpkgs-fmt
    ormolu
    ruby
  ];
}
