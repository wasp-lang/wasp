#!/usr/bin/env bash

SCRIPT_DIR=$(dirname "$(realpath "$0")")
# Assumes this script is in `opensaas-sh/tools/`.
ROOT_DIR="${SCRIPT_DIR}/../.."

cd "${ROOT_DIR}"

rm -rf opensaas-sh/app_diff
"${ROOT_DIR}/tools/dope.sh" template/app opensaas-sh/app diff
