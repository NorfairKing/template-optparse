{ mkDerivation, autodocodec, autodocodec-yaml, base, envparse
, hspec, lib, optparse-applicative, path, path-io, text, yaml
}:
mkDerivation {
  pname = "template-optparse";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    autodocodec autodocodec-yaml base envparse optparse-applicative
    path path-io text yaml
  ];
  testHaskellDepends = [
    base envparse hspec optparse-applicative yaml
  ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
