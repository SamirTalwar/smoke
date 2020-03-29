{ mkDerivation
, aeson
, ansi-terminal
, base
, containers
, data-default
, Diff
, directory
, exceptions
, filepath
, Glob
, hedgehog
, hspec
, hspec-discover
, hw-hspec-hedgehog
, mtl
, optparse-applicative
, process
, process-extras
, stdenv
, tar
, temporary
, text
, transformers
, unix
, vector
, yaml
}:
mkDerivation {
  pname = "smoke";
  version = "2.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    containers
    data-default
    directory
    exceptions
    filepath
    Glob
    process
    process-extras
    tar
    temporary
    text
    transformers
    vector
    yaml
  ];
  executableHaskellDepends = [
    aeson
    ansi-terminal
    base
    containers
    data-default
    Diff
    directory
    exceptions
    filepath
    Glob
    mtl
    optparse-applicative
    process
    process-extras
    tar
    temporary
    text
    transformers
    unix
    vector
    yaml
  ];
  testHaskellDepends = [
    aeson
    base
    containers
    data-default
    directory
    exceptions
    filepath
    Glob
    hedgehog
    hspec
    hw-hspec-hedgehog
    process
    process-extras
    tar
    temporary
    text
    transformers
    vector
    yaml
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/SamirTalwar/smoke#readme";
  description = "An integration test framework for console applications";
  license = stdenv.lib.licenses.mit;
}
