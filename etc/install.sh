#!/bin/bash
# change into the repo's root directory
cd "$(dirname "$0")"
cd ..

EXEC="./dist/build/spacecookie/spacecookie"
PREFIX="/usr/local"

test -e "$EXEC" || (echo "Use etc/build.sh to build spacecookie first!"; exit 1)

install -d "$PREFIX/bin"
install -m755 "$EXEC" "$PREFIX/bin"
install -d "$PREFIX/share/licenses/spacecookie"
install -m644 "LICENSE" "$PREFIX/share/licenses/spacecookie"
install -m644 "example.yaml" "/etc/spacecookie.yaml"

echo "To uninstall spacecookie run 'rm $PREFIX/bin/spacecookie $PREFIX/share/licenses/spacecookie/LICENSE /etc/spacecookie.yaml'"
