#!/bin/sh -e

# Takes last waspls binary built by cabal and packages it into .tar.gz package.

# First and only argument is the filename of the package to be generated.
# It is optional.

DST=$PWD/${1:-waspls.tar.gz}
TMP_DIR="$(mktemp -d 2>/dev/null || mktemp -d -t waspls-bin-package)"
WASPLS_BINARY_PATH="$(cabal list-bin waspls:waspls)"

cp "$WASPLS_BINARY_PATH" "$TMP_DIR/waspls-bin"
cd "$TMP_DIR" && tar -czf "$DST" *
if [ -n "$TMP_DIR" ]; then rm -rf "$TMP_DIR"; fi

echo "Generated binary package: $DST."
