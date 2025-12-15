#!/usr/bin/env bash

# Bash "Strict Mode" http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
set -x # Log the commands being run, for easy debugging.

# Takes last wasp binary built by cabal and packages it together with data
# into .tar.gz package.

# First and only argument is the filename of the package to be generated.
# It is optional.

DST=$PWD/${1:-wasp.tar.gz}

TMP_DIR="$(mktemp -d 2> /dev/null || mktemp -d -t wasp-bin-package)"
CABAL_PROJECT_ROOT_PATH="$(cabal list-bin wasp-cli | sed s/\\/dist-newstyle.*//)"

WASP_BINARY_PATH="$(cabal list-bin wasp-cli)"
cp "$WASP_BINARY_PATH" "$TMP_DIR/wasp-bin"

CABAL_DATA_FILE_PATHS=$(
  # First we list all the files declared in the .cabal file
  # Then we only take the ones in the "data" directory
  cabal sdist --list-only | grep "^./data/"
)
for data_file_path in $CABAL_DATA_FILE_PATHS; do
  source_path="$CABAL_PROJECT_ROOT_PATH/$data_file_path"
  dest_path="$TMP_DIR/$data_file_path"

  mkdir -p "$(dirname "$dest_path")" # Make sure the parent directories in the output exist.
  cp "$source_path" "$dest_path"
done

tar -czf "$DST" -C "$TMP_DIR" .

if [ -n "$TMP_DIR" ]; then rm -rf "$TMP_DIR"; fi

echo "Generated binary package: $DST."
