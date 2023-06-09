#!/bin/bash -e

# Helper to compile the waspc/packages/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).

# Gets the directory of where this script lives.
dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

for package in $(ls "$dir/../packages"); do
  package_dir="$dir/../packages/$package"
  if [[ -d "$package_dir" ]]; then
    echo "Installing $package ($package_dir)"
    cd "$package_dir"
    npm install
    npm run build
  fi
done

cd "$dir/.."
rm -rf ./data/packages
cp -R ./packages ./data
