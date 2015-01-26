# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, aeson, attoparsec, blazeBuilder, Cabal, caseInsensitive
, dataDefault, either, httpTypes, mime, mtl, network, networkUri
, optparseApplicative, postgresqlSimple, QuickCheck, scotty, text
, time, transformers, unorderedContainers, vector, wai, warp
}:

cabal.mkDerivation (self: {
  pname = "youdo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec Cabal dataDefault either httpTypes mime mtl
    network networkUri optparseApplicative postgresqlSimple scotty text
    time transformers unorderedContainers warp
  ];
  testDepends = [
    aeson attoparsec blazeBuilder Cabal caseInsensitive either
    httpTypes mtl network networkUri QuickCheck scotty text time
    transformers unorderedContainers vector wai
  ];
  meta = {
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
  enableSharedExecutables = false;
})
