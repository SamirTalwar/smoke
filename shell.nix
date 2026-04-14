{ system ? builtins.currentSystem
, sources ? import ./nix/npins
, pkgs ? import sources.nixpkgs { inherit system; }
, haskell ? import ./nix/haskell.nix { inherit system sources pkgs; }
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    coreutils
    dos2unix
    findutils
    gnumake
    gnused
    npins
    nixpkgs-fmt
    yq

    # Haskell development
    haskell.packages.haskell-language-server
    haskell.packages.hlint
    haskell.packages.hpack
    haskell.packages.hspec-discover
    haskell.packages.ormolu
    stack

    # testing
    git
    ruby
  ];
  STACK_IN_NIX_SHELL = true;
}
