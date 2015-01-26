YouDo Server
============

## Using Nix

Using nix requires you to checkout the `cabal-test-quickcheck` package from source as it's not provided by nixpkgs.

Running `nix-shell run.nix` will build, test, and place you in an environment with `youdo` and `postgresql`.
