#!/bin/bash
# change into the repo's root directory
cd "$(dirname "$0")"
cd ..

test -d .cabal-sandbox && cabal sandbox delete

cabal sandbox init
cabal install --only-dependencies -j2
cabal build && \
	echo "spacecookie was successfully built! Now use etc/install.sh to install it"
