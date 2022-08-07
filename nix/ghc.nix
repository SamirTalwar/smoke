{ lib
, haskell
, ghcVersion ? lib.strings.fileContents ../ghc.version
, overrides ? _: _: { }
}:
let
  compiler = "ghc" + lib.strings.stringAsChars (c: if c == "." then "" else c) ghcVersion;
in
haskell.packages."${compiler}".override {
  inherit overrides;
}
