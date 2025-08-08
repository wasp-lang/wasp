#!/bin/bash -e

# Helper to compile the waspc/libs/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).

# Gets the directory of where this script lives.
dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# Cleanup old libs
cd "$dir/.."
rm -f ./data/Generator/libs/*.tgz

# Build and copy libs to data dir
for lib in $(ls "$dir/../libs"); do
  lib_dir="$dir/../libs/$lib"
  if [[ -d "$lib_dir" ]]; then
    echo "Installing $lib lib ($lib_dir)"
    cd "$lib_dir"
    npm install
    npm run pack-and-copy
  fi
done
