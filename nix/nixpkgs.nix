{}:
let
  config = {};

  overlays = [];

  nixpkgs-unstable = import (
    builtins.fetchTarball {
      url = https://github.com/NixOS/nixpkgs-channels/archive/05f0934825c2a0750d4888c4735f9420c906b388.tar.gz;
      sha256 = "1g8c2w0661qn89ajp44znmwfmghbbiygvdzq0rzlvlpdiz28v6gy";
    }
  );
in
nixpkgs-unstable { inherit config overlays; }
