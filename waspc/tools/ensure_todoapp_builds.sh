#!/bin/sh -e

# Compiles the todoApp and checks `npm run build` on both client and server.
# This helps us out in CI since Vite won't typecheck locally. So we want to
# make sure we don't accidentally add anything that causes `tsc` to error out.

# Gets the directory of where this script lives.
dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
echo "Running ensure_todoapp_builds.sh from $dir"

cd "$dir/../examples/todoApp"

# Compile example app.
cabal run wasp-cli build

# Make sure they build.
echo "Ensure client builds"
cd .wasp/build/web-app
npm run build

echo "Ensure server builds"
cd ../server
npm run build
