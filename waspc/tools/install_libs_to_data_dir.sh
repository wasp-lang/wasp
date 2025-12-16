#!/bin/bash -e

# Helper to compile the waspc/libs/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).

script_dir=$(CDPATH="" cd -- "$(dirname -- "$0")" && pwd)
waspc_dir=$script_dir/..
data_libs_dir=$waspc_dir/data/Generator/libs

waspc_version=$("$waspc_dir/run" get-waspc-version)

# Clean up old libs.
rm -rf "${data_libs_dir:?data_libs_dir must be set before cleanup}"
mkdir -p "$data_libs_dir"

# Build and copy libs to data dir.
for lib_dir in "$waspc_dir"/libs/*; do
  if [[ -d "$lib_dir" ]]; then
    cd "$lib_dir"
    lib_name=$(jq -r '.name' package.json)
    lib_version=$(jq -r '.version' package.json)
    echo "Installing $lib_name lib ($lib_dir)"

    if [[ "$lib_version" != "$waspc_version" ]]; then
      echo "ERROR: $lib_name lib version ($lib_version) != current Wasp version ($waspc_version)."
      echo "       Update the lib version in package.json to $waspc_version."
      exit 1
    fi

    npm install
    # Clean up old lib tarballs.
    rm -f ./*.tgz
    npm pack
    cp ./*.tgz "$data_libs_dir"
  fi
done
