#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
WASPELLO_DIR="$SCRIPT_DIR/../waspello"
WASPC_DIR="$SCRIPT_DIR/../../waspc"
LW=("$WASPC_DIR/run" "wasp-cli")

BOLD="\033[1m"
RESET="\033[0m"
GREEN="\033[32m"
LIGHT_CYAN="\033[96m"

step() { echo -e "${LIGHT_CYAN}${BOLD}==> $1${RESET}"; }

command -v jq >/dev/null || { echo "jq is required"; exit 1; }

# 1. Compile, build, and pack all modules
for pkg in "$SCRIPT_DIR"/*/package.json; do
  dir=$(dirname "$pkg")
  name=$(basename "$dir")
  step "Installing dependencies for $name"
  (cd "$dir" && npm install)
  step "Building $name"
  (cd "$dir" && "${LW[@]}" module build && npx tsc -p tsconfig.build.json)
  step "Packing $name"
  (cd "$dir" && npm pack)
done

# 2. Bust module cache in waspello's package-lock.json
step "Busting module cache in package-lock.json"
PACKAGE_LOCK="$WASPELLO_DIR/package-lock.json"
PACKAGE_LOCK_TMP=$(mktemp)
if ! jq '.packages |= with_entries(select(.key | test("^node_modules/@waspello/") | not))' "$PACKAGE_LOCK" > "$PACKAGE_LOCK_TMP"; then
  rm -f "$PACKAGE_LOCK_TMP"
  exit 1
fi
mv "$PACKAGE_LOCK_TMP" "$PACKAGE_LOCK"

# 3. Remove installed modules and reinstall
step "Reinstalling in waspello"
rm -rf "$WASPELLO_DIR/node_modules/@waspello"
(cd "$WASPELLO_DIR" && npm install --force)

echo -e "${GREEN}${BOLD}Done!${RESET}"
