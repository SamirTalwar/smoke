let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  deps = import ./nix/deps.nix { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = deps;

  NIX_PATH = "nixpkgs=" + sources.nixpkgs;
}
