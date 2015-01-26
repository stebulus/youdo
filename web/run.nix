{ nixpkgs ? (import <nixpkgs> {})
, haskell ? nixpkgs.haskellPackages
, cabalTestQuickcheck ? haskell.callPackage (import ../../cabal-test-quickcheck/cabal-test-quickcheck.nix) { Cabal = haskell.Cabal_1_20_0_3; }
, youdo ? haskell.callPackage (import ./youdo.nix) { cabalTestQuickcheck = cabalTestQuickcheck;
                                                     attoparsec = haskell.attoparsec_0_11_3_1;
                                                   }
}:
let
  myEnvFun = nixpkgs.myEnvFun;
  postgresql = nixpkgs.postgresql;
in rec {
  youdoRuntimeEnv = myEnvFun {
    name = "youdo-runtime-env";

    buildInputs = [
      haskell.cabalInstall
      postgresql
      youdo
    ];
  };
}
