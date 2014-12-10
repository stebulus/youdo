{ nixpkgs ? (import <nixpkgs> {}) }:
let
  stdenv = nixpkgs.stdenv;
  node = nixpkgs.nodePackages;
in stdenv.mkDerivation {

  name = "youdo";
  buildInputs = [
    node.browserify
    node.npm
  ];
}

