let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };
  deps = import ./deps.nix { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = deps ++ [ pkgs.nix ];

  CI = "true";
  NIX_PATH = "nixpkgs=" + sources.nixpkgs;
}
