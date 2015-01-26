{ nixpkgs ? (import <nixpkgs> {})
, haskell ? nixpkgs.haskellPackages
}:
let
  myEnvFun = nixpkgs.myEnvFun;
  postgresql = nixpkgs.postgresql;

  cabalInstall = haskell.cabalInstall_1_20_0_6;
  ghc = haskell.ghc;
  ghcmod = haskell.ghcMod;
  Cabal = haskell.Cabal_1_20_0_3;

  cabal = haskell.cabal;
  aeson = haskell.aeson;
  attoparsec = haskell.attoparsec_0_11_3_1;
  blazeBuilder = haskell.blazeBuilder;
  caseInsensitive = haskell.caseInsensitive;
  dataDefault = haskell.dataDefault;
  either = haskell.either;
  httpTypes = haskell.httpTypes;
  mime = haskell.mime;
  mtl = haskell.mtl;
  network = haskell.network;
  networkUri = haskell.networkUri;
  optparseApplicative = haskell.optparseApplicative;
  postgresqlSimple = haskell.postgresqlSimple;
  QuickCheck = haskell.QuickCheck;
  scotty = haskell.scotty;
  text = haskell.text;
  time = haskell.time;
  transformers = haskell.transformers;
  unorderedContainers = haskell.unorderedContainers;
  vector = haskell.vector;
  wai = haskell.wai;
  warp = haskell.warp;
in rec {
  youdoRuntimeEnv = myEnvFun {
    name = "youdo-dev-env";

    buildInputs = [
      postgresql
      ghc
      ghcmod
      cabalInstall
      Cabal

      aeson
      attoparsec
      blazeBuilder
      caseInsensitive
      dataDefault
      either
      httpTypes
      mime
      mtl
      network
      networkUri
      optparseApplicative
      postgresqlSimple
      QuickCheck
      scotty
      text
      time
      transformers
      unorderedContainers
      vector
      wai
      warp
    ];
  };
}
