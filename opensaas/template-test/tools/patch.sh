#!/usr/bin/env bash

set -e

command -v wasp > /dev/null || {
  echo "Error: Wasp CLI not found"
  exit 1
}

SCRIPT_DIR=$(dirname "$(realpath "$0")")
ROOT_DIR="${SCRIPT_DIR}/../.."
BASE_DIR="${ROOT_DIR}/template-test/base-app"

# Clean up the temporary base app directory on exit.
trap 'rm -rf "${BASE_DIR}"' EXIT

cd "${ROOT_DIR}"

rm -rf "${BASE_DIR}"
(cd "${ROOT_DIR}/template-test" && wasp new -t saas base-app)
(cd "${BASE_DIR}/app" && git init -b main -q && git add .)

# Clean up existing derived app directory if it exists.
rm -rf template-test/app

"${ROOT_DIR}/tools/dope.sh" "${BASE_DIR}/app" template-test/app patch
