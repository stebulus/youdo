{ nixpkgs ? (import <nixpkgs> {}) }:
let
  stdenv = nixpkgs.stdenv;
  node = nixpkgs.nodePackages;
  python = nixpkgs.python27;
in stdenv.mkDerivation {

  name = "youdo";
  buildInputs = [
    node.browserify
    node.npm
    python
  ];
}

