{ nixpkgs ? (import <nixpkgs> {}) }:
let
  myEnvFun = nixpkgs.myEnvFun;
  postgresql = nixpkgs.postgresql;
in rec {
  youdoDB = myEnvFun {
    name = "youdo-db-env";
    buildInputs = [
      postgresql
    ];
  }; 
}
