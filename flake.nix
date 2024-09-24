{
  description = "Smoke";

  inputs = {
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self
    , flake-compat
    , flake-utils
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      ghcVersion = lib.strings.fileContents ./ghc.version;
      ghcName = "ghc" + lib.strings.stringAsChars (c: if c == "." then "" else c) ghcVersion;
      pkgs = import nixpkgs { inherit system; };

      inherit (pkgs) lib haskell;
      inherit (haskell.lib) overrideCabal justStaticExecutables doStrip;
      hsPkgs = haskell.packages.${ghcName};

      # required libraries with static linking enabled
      staticLibs = with pkgs; [
        (gmp6.override { withStatic = true; })
        (libffi.overrideAttrs (old: { dontDisableStatic = true; }))
        (libiconv.overrideAttrs (old: { enableStatic = true; enableShared = false; }))
        (openssl.override { static = true; })
        zlib.static
      ];

      smoke =
        let
          drv = hsPkgs.callCabal2nix "smoke" (pkgs.nix-gitignore.gitignoreSource [ ] ./.) {
            # Override tar with the patched version; see stack.yaml for details.
            tar = hsPkgs.tar_0_6_3_0;
          };
        in
        haskell.lib.addExtraLibraries drv staticLibs;
    in
    {
      packages.default = smoke;

      apps.default = flake-utils.lib.mkApp {
        drv = overrideCabal (justStaticExecutables (doStrip smoke)) {
          enableSharedExecutables = false;
          enableSharedLibraries = false;
        };
      };

      formatter = pkgs.nixpkgs-fmt;

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          coreutils
          dos2unix
          findutils
          gnumake
          gnused
          nixpkgs-fmt
          yq

          # Haskell development
          hsPkgs.haskell-language-server
          hsPkgs.hlint
          hsPkgs.hpack
          hsPkgs.hspec-discover
          hsPkgs.ormolu
          stack

          # testing
          git
          ruby
        ];
        STACK_IN_NIX_SHELL = true;
      };

      devShells.lint = pkgs.mkShell {
        buildInputs = with pkgs; [
          hsPkgs.hlint
          hsPkgs.hpack
          hsPkgs.ormolu
        ];
        STACK_IN_NIX_SHELL = true;
      };
    });
}
