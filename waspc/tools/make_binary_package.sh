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

# This pipeline of three commands copies the data files declared in the Cabal
# file into the tmp dir to be packaged.
#
# 1. List all the files considered by Cabal to be part of the source (including
# the declared data files).
# 2. Filter to get only the files in the "data" dir.
# 3. Copy the files in that list from the project root to the tmp dir to be
# packaged.
#
# We use rsync to recreate the directory structure properly, plus it has a nice
# `--files-from=-` option to read from stdin the list of files to be copied.
cabal sdist --list-only \
  | grep "^\./data/" \
  | rsync -av --files-from=- "$CABAL_PROJECT_ROOT_PATH" "$TMP_DIR"

tar -czf "$DST" -C "$TMP_DIR" .

if [ -n "$TMP_DIR" ]; then rm -rf "$TMP_DIR"; fi

echo "Generated binary package: $DST."
