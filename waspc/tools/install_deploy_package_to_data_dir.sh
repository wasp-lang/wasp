#!/bin/sh -e

# Helper to compile the waspc/packages/deploy package locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).

# Gets the directory of where this script lives.
dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

cd "$dir/../packages/deploy"
npm install
npm run build
rm -rf ./node_modules

cd "$dir/.."
rm -rf ./data/packages
cp -R ./packages ./data
