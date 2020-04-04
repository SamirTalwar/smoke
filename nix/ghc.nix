{ lib, haskell }:
let
  version = lib.strings.fileContents ../ghc.version;
  compiler = "ghc" + lib.strings.stringAsChars (c: if c == "." then "" else c) version;
in
haskell.packages."${compiler}"
