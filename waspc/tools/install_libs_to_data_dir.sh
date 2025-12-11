#!/bin/bash -e

# Helper to compile the waspc/libs/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).

script_dir=$(CDPATH="" cd -- "$(dirname -- "$0")" && pwd)
waspc_dir=$script_dir/..
data_libs_dir=$waspc_dir/data/Generator/libs

# Clean up old libs.
rm -rf "${data_libs_dir:?data_libs_dir must be set before cleanup}"
mkdir -p "$data_libs_dir"

# Build and copy libs to data dir.
for lib_dir in "$waspc_dir"/libs/*; do
  if [[ -d "$lib_dir" ]]; then
    lib=$(basename "$lib_dir")
    echo "Installing $lib lib ($lib_dir)"
    cd "$lib_dir"
    npm install
    npm run build

    # Copy only necessary files (not node_modules, test files, etc.)
    # We copy: package.json, dist/, and any other essential files
    dest_lib_dir="$data_libs_dir/$lib"
    mkdir -p "$dest_lib_dir"
    cp package.json "$dest_lib_dir/"
    if [[ -d dist ]]; then
      cp -r dist "$dest_lib_dir/"
    fi
    if [[ -f README.md ]]; then
      cp README.md "$dest_lib_dir/"
    fi
  fi
done
