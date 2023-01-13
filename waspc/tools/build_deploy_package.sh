#!/bin/sh -e

# Builds the waspc/packages/deploy package locally and in CI.

# Gets the directory of where this script lives.
dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

cd "$dir/../packages/deploy"
npm install
npm run build
rm -rf ./node_modules

cd "$dir/.."
rm -rf ./data/packages
cp -R ./packages ./data
