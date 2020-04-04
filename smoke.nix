{ ghc, nix-gitignore }:
ghc.callCabal2nix "smoke" (nix-gitignore.gitignoreSource [] ./.) {}
