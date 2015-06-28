#!/bin/bash
# change into the repo's root directory
cd "$(dirname "$0")"
cd ..

test -d ./.cabal-sandbox && cabal sandbox delete
test -d ./dist && rm -rf ./dist

nix-build -v build.nix
