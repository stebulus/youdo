YouDo FrontEnd
==============

# Building

0. Install the [nix package manager](https://nixos.org/nix)
1. `nix-shell` (this will install all the dependencies required for building).
2. Install the node modules
  1. `npm install global`
  2. `npm install mercury`
2. `browserify app.js -o bundle.js`
3. Open `index.html` in your browser. Kapow.

## How this Works?

Some javascript modules are stored in `npm_modules`. When you run `browserify`, anytime you have a `require('x');` statement in a javascript file, `browserify` will include that and bundle all the javascript into a single file.

If you want to add or use a javascript package, ensure it's in the [CommonJS](http://wiki.commonjs.org/wiki/CommonJS)/[RequireJS](https://requirejs.org) format, and just install via `npm` ala: `npm install my-fav-lib`.
