#!/bin/sh -e

# Takes last wasp binary built by cabal and packages it together with data
# into .tar.gz package.

# First and only argument is the filename of the package to be generated.
# It is optional.

DST=$PWD/${1:-wasp.tar.gz}

TMP_DIR="$(mktemp -d 2>/dev/null || mktemp -d -t wasp-bin-package)"

WASP_BINARY_PATH="$(cabal list-bin wasp-cli)"
cp "$WASP_BINARY_PATH" "$TMP_DIR/wasp-bin"

CABAL_PROJECT_ROOT_PATH="$(cabal list-bin wasp-cli | sed s/\\/dist-newstyle.*//)"
cp -R "$CABAL_PROJECT_ROOT_PATH/data" "$TMP_DIR/data"

cd "$TMP_DIR"
tar -czf "$DST" *

if [ -n "$TMP_DIR" ]; then rm -rf "$TMP_DIR"; fi

echo "Generated binary package: $DST."
