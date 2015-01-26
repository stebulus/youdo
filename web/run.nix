{ nixpkgs ? (import <nixpkgs> {})
, haskell ? nixpkgs.haskellPackages
, youdo ? haskell.callPackage (import ./youdo.nix) { Cabal = haskell.Cabal_1_20_0_3;
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
      postgresql
      youdo
    ];
  };
}
