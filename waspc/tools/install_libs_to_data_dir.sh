#!/bin/bash -e

# Helper to compile the waspc/libs/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).

script_dir=$(CDPATH="" cd -- "$(dirname -- "$0")" && pwd)
waspc_dir=$script_dir/..
data_libs_dir=$waspc_dir/data/Generator/libs

# Get the waspc version to use for the libs.
if [[ -n "$USE_RANDOM_LIB_VERSION" ]]; then
  # Use a random version for cache busting in development
  lib_version="0.0.0-dev-$(openssl rand -hex 8)"
  echo "USE_RANDOM_LIB_VERSION is set - using random version for cache busting: $lib_version"
else
  lib_version=$("$waspc_dir/run" get-waspc-version | tail -n 1)
  echo "Setting lib versions to: $lib_version"
fi

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

    # Extract the package name prefix from the original tarball
    # e.g., wasp.sh-lib-auth-0.0.0.tgz -> wasp.sh-lib-auth
    tarball_prefix="${original_tarball_file_name%-*.tgz}"

    new_tarball_file_name="${tarball_prefix}-${lib_version}.tgz"

    if [[ "$original_tarball_file_name" != "$new_tarball_file_name" ]]; then
      mv "$original_tarball_file_name" "$new_tarball_file_name"
      echo "Renamed $original_tarball_file_name -> $new_tarball_file_name"
    else
      echo "Tarball already has the correct name: $new_tarball_file_name"
    fi

    jq --arg pkg "$package_name" --arg tar "$new_tarball_file_name" '. + {($pkg): $tar}' "$manifest_file" > "$manifest_file.tmp" && mv "$manifest_file.tmp" "$manifest_file"

    cp "$new_tarball_file_name" "$data_libs_dir"
  fi
done

echo "Generated manifest at: $manifest_file"
