{ system ? builtins.currentSystem
, sources ? import ./nix/npins
, pkgs ? import sources.nixpkgs { inherit system; }
, ghcVersion ? pkgs.lib.strings.fileContents ./ghc.version
, haskell ? import ./nix/haskell.nix { inherit system sources pkgs ghcVersion; }
}:
let
  # required libraries with static linking enabled
  staticLibs = with pkgs; [
    (gmp6.override { withStatic = true; })
    (libffi.overrideAttrs (old: {
      dontDisableStatic = true;
    }))
    (libiconv.overrideAttrs (old: {
      enableStatic = true;
      enableShared = false;
    }))
    (openssl.override { static = true; })
    zlib.static
  ];

  drv = haskell.packages.callCabal2nix "smoke" (pkgs.nix-gitignore.gitignoreSource [ ] ./.) {
    # Override tar with the patched version; see stack.yaml for details.
    tar = haskell.packages.tar_0_6_3_0;
  };
in
haskell.lib.addExtraLibraries drv staticLibs
