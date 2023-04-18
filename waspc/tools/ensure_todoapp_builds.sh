#!/bin/sh -e

# Compiles the todoApp and checks `npm run build` on both client and server.

# Gets the directory of where this script lives.
dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
echo "Running ensure_todoapp_builds.sh from $dir"

cd "$dir/../examples/todoApp"
echo "cd into $(pwd)"

# Compile example app.
WASP_BINARY_PATH="$(cabal list-bin wasp-cli)"
CABAL_PROJECT_ROOT_PATH="$(cabal list-bin wasp-cli | sed s/\\/dist-newstyle.*//)"
WASP_COMMAND="waspc_datadir=$CABAL_PROJECT_ROOT_PATH/data $WASP_BINARY_PATH"
echo "Wasp command: $WASP_COMMAND"

eval "$WASP_COMMAND build"

# Make sure they build.
echo "Ensure client builds"
cd .wasp/build/web-app
npm run build

echo "Ensure server builds"
cd ../server
npm run build
