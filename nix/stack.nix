let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };
  ghc = import ./ghc.nix { inherit (pkgs) lib haskell; };
  deps = import ./deps.nix { inherit pkgs; };
  extraLibraries = import ./libs.nix { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = deps ++ [ ghc.ghc ] ++ extraLibraries;

  # Necessary until https://github.com/commercialhaskell/stack/issues/5008 is fixed.
  STACK_IN_NIX_SHELL = true;
}
