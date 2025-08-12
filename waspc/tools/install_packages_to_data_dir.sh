#!/bin/bash -e

# Helper to compile the waspc/packages/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).

# Gets the directory of where this script lives.
script_dir=$(CDPATH="" cd -- "$(dirname -- "$0")" && pwd)

for package_dir in "$script_dir"/../packages/*; do
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

cd "$script_dir/.."
rm -rf ./data/packages
cp -R ./packages ./data
