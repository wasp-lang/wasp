#!/bin/bash -e

# Helper to compile the waspc/libs/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).

script_dir=$(CDPATH="" cd -- "$(dirname -- "$0")" && pwd)
waspc_dir=$script_dir/..
data_libs_dir=$waspc_dir/data/Generator/libs

# Clean up old libs.
rm -rf "${data_libs_dir:?data_libs_dir must be set before cleanup}"
mkdir -p "$data_libs_dir"

# Initialize manifest
manifest_file="$data_libs_dir/manifest.json"
echo '{}' > "$manifest_file"

# Build and copy libs to data dir.
for lib_dir in "$waspc_dir"/libs/*; do
  if [[ -d "$lib_dir" ]]; then
    lib=$(basename "$lib_dir")
    echo "Installing $lib lib ($lib_dir)"
    cd "$lib_dir"
    npm install
    # Clean up old lib tarballs.
    rm -f ./*.tgz
    npm pack

    package_name=$(jq -r '.name' package.json)
    original_tarball_file_name=$(basename ./*.tgz)

    if [[ -n "$USE_RANDOM_LIB_VERSION" ]]; then
      tarball_prefix="${original_tarball_file_name%-*.tgz}"
      random_lib_version="0.0.0-dev-$(openssl rand -hex 8)"
      new_tarball_file_name="${tarball_prefix}-${random_lib_version}.tgz"
      mv "$original_tarball_file_name" "$new_tarball_file_name"
      echo "Renamed $original_tarball_file_name -> $new_tarball_file_name"
    else
      new_tarball_file_name="$original_tarball_file_name"
    fi

    jq --arg pkg "$package_name" --arg tar "$new_tarball_file_name" '. + {($pkg): $tar}' "$manifest_file" > "$manifest_file.tmp" && mv "$manifest_file.tmp" "$manifest_file"

    cp "$new_tarball_file_name" "$data_libs_dir"
  fi
done

echo "Generated manifest at: $manifest_file"
