cd "$(dirname "$0")"
EXEC="./dist/build/spacecookie/spacecookie"
PREFIX="/usr/local"

test -e "$EXEC" || (echo "Build spacecookie first"; exit 1)

install -m755 "$EXEC" "$PREFIX/bin"

echo "To uninstall spacecookie run 'rm $PREFIX/bin/spacecookie'"
