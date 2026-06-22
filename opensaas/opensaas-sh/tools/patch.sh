#!/usr/bin/env bash

SCRIPT_DIR=$(dirname "$(realpath "$0")")
# Assumes this script is in `opensaas-sh/tools/`.
ROOT_DIR="${SCRIPT_DIR}/../.."

cd "${ROOT_DIR}"

# Removes all files except for some gitignored files that we don't want to bother regenerating each time,
# like node_modules and certain .env files.
find opensaas-sh/app -mindepth 1 \( -path node_modules -o -name .env.server -o -name .env.me \) -prune -o -exec rm -rf {} +
"${ROOT_DIR}/tools/dope.sh" template/app opensaas-sh/app patch
