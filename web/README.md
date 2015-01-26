YouDo Server
============

## Using Nix

1. Running `nix-shell dev.nix` will put you in a development environment with: `ghc-mod`, `ghc`, `cabal` and the necessary libraries to work with `youdo-server`.
2. Running `nix-shell run.nix` will build, test, and place you in an environment with `youdo` and `postgresql`.
