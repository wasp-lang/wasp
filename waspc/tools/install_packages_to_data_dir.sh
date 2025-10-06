#!/bin/bash -e

# Helper to compile the waspc/packages/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).

# Gets the directory of where this script lives.
script_dir=$(CDPATH="" cd -- "$(dirname -- "$0")" && pwd)
waspc_dir=$script_dir/..
data_packages_dir=$waspc_dir/data/packages

# Cleanup old packages in data dir.
rm -rf "${data_packages_dir:?data_packages_dir must be set before cleanup}"
mkdir -p "$data_packages_dir"

for package_dir in "$waspc_dir"/packages/*; do
  if [[ -d "$package_dir" ]]; then
    package=$(basename "$package_dir")
    # We're only installing the dependencies here to verify that the build
    # works, that's why the node_modules folder is removed immediately after.
    # The real dependency installation happens in Haskell.
    echo "Installing $package ($package_dir)"
    cd "$package_dir"
    npm install
    npm run build
    rm -rf ./node_modules
  fi
done

cd "$waspc_dir"
cp -R ./packages/* "$data_packages_dir"
