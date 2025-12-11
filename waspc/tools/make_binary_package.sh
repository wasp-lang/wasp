#!/usr/bin/env bash

set -euxo pipefail

# Takes last wasp binary built by cabal and packages it together with data
# into .tar.gz package.

# First and only argument is the filename of the package to be generated.
# It is optional.

DST=$PWD/${1:-wasp.tar.gz}

TMP_DIR="$(mktemp -d 2> /dev/null || mktemp -d -t wasp-bin-package)"
CABAL_PROJECT_ROOT_PATH="$(cabal list-bin wasp-cli | sed s/\\/dist-newstyle.*//)"

WASP_BINARY_PATH="$(cabal list-bin wasp-cli)"
cp "$WASP_BINARY_PATH" "$TMP_DIR/wasp-bin"

cabal sdist --list-only \
  | grep "^./data" \
  | while read -r data_file_path; do
    mkdir -p "$TMP_DIR/$(dirname "$data_file_path")"
    cp "$CABAL_PROJECT_ROOT_PATH/$data_file_path" "$TMP_DIR/$data_file_path"
  done

tar -czf "$DST" -C "$TMP_DIR" .

if [ -n "$TMP_DIR" ]; then rm -rf "$TMP_DIR"; fi

echo "Generated binary package: $DST."
