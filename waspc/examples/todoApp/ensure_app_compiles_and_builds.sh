#!/bin/sh -e

# Compiles the todoApp and checks `npm run build` on both client and server.
# This helps us out in CI since Vite won't typecheck locally. So we want to
# make sure we don't accidentally add anything that causes `tsc` to error out.

# Gets the directory of where this script lives.
dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
echo "Running ensure_app_compiles_and_builds.sh from $dir"

cd "$dir"

# Compile example app.
cabal run wasp-cli build

echo "Ensure the user's code has no TypeScript errors (shown in the IDE)."
npx tsc --noEmit --project .

# Make sure they build.
echo "Ensure client builds"
cd .wasp/build/web-app
npm run build
npx tsc --noEmit --project .

echo "Ensure server bundles"
cd ../server
npm run bundle
npx tsc --noEmit --project .
