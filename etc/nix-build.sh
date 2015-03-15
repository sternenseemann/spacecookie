#!/bin/bash
# change into the repo's root directory
cd "$(dirname "$0")"
cd ..

test -d .cabal-sandbox && nix-shell --pure --command 'cabal sandbox delete'

nix-shell --pure --command 'cabal configure && cabal build'
