#!/bin/bash -e

# Helper to compile the waspc/libs/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
waspc_dir=$script_dir/..
data_libs_dir=$waspc_dir/data/Generator/libs

# Cleanup old libs
cd "$script_dir/.."
rm -f $data_libs_dir/*.tgz

# Build and copy libs to data dir
for lib in $(ls "$script_dir/../libs"); do
  lib_dir="$script_dir/../libs/$lib"
  if [[ -d "$lib_dir" ]]; then
    echo "Installing $lib lib ($lib_dir)"
    cd "$lib_dir"
    npm install
    # Cleanup old tarballs
    rm -f *.tgz
    npm pack
    cp *.tgz $data_libs_dir
  fi
done
