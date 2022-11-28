{
  description = "Smoke";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    nixpkgs.url = github:NixOS/nixpkgs/master;
    haskellTar.url = github:haskell/tar/dbf8c995153c8a80450724d9f94cf33403740c80;
    haskellTar.flake = false;
    flake-compat.url = github:edolstra/flake-compat;
    flake-compat.flake = false;
    pre-commit-hooks.url = github:cachix/pre-commit-hooks.nix;
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
    pre-commit-hooks.inputs.flake-compat.follows = "flake-compat";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , haskellTar
    , flake-compat
    , pre-commit-hooks
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      inherit (pkgs) lib haskell;
      inherit (haskell.lib) overrideCabal justStaticExecutables doStrip;

      # required libraries with static linking enabled
      staticLibs = with pkgs; [
        (gmp6.override { withStatic = true; })
        (libffi.overrideAttrs (old: { dontDisableStatic = true; }))
        (libiconv.overrideAttrs (old: { enableStatic = true; enableShared = false; }))
        (openssl.override { static = true; })
        zlib.static
      ];

      # haskell package set from ghc specified by `./ghc.version` and
      # tar package with tests disabled
      hsPkgs =
        let
          ghcVersion = lib.strings.fileContents ./ghc.version;
          compiler = "ghc" + lib.strings.stringAsChars (c: if c == "." then "" else c) ghcVersion;
          overrides = hself: hsuper: { tar = hsuper.callCabal2nixWithOptions "tar" haskellTar "--no-check" { }; };
        in
        haskell.packages."${compiler}".override { inherit overrides; };

      smoke =
        let
          drv = hsPkgs.callCabal2nix "smoke" (pkgs.nix-gitignore.gitignoreSource [ ] ./.) { };
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

      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixpkgs-fmt.enable = true;
            ormolu.enable = true;
            hlint.enable = true;
          };
        };
      };

      formatter = pkgs.nixpkgs-fmt;

      devShells.default = pkgs.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
        buildInputs = with pkgs; [
          coreutils
          dos2unix
          findutils
          git
          gnumake
          gnused
          hsPkgs.haskell-language-server
          hsPkgs.hlint
          hsPkgs.hspec-discover
          nixpkgs-fmt
          ormolu
          ruby
          stack
          yq
        ];
        STACK_IN_NIX_SHELL = true;
      };
    });
}
