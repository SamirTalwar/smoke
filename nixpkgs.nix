{}:
let
  config = {};

  overlays = [];

  nixpkgs-unstable = import (
    builtins.fetchTarball {
      url = https://github.com/NixOS/nixpkgs/archive/6a3ce692c0bdc393e2ae3a98b30778b86e0708b0.tar.gz;
      sha256 = "1cf88dadrk8m3447skl6zrldn2r59j4dzygsbi94myvq128h83mw";
    }
  );
in
nixpkgs-unstable { inherit config overlays; }
