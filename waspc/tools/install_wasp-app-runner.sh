#!/usr/bin/env bash

# This script ensures wasp-app-runner is installed and up-to-date.

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
PROJECT_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)

RESET="\033[0m"
YELLOW="\033[33m"
GREEN="\033[32m"
RED="\033[31m"

main_flow() {
  echo -e "Checking wasp-app-runner status..."

  if ! is_wasp_app_runner_installed; then
    echo -e "${YELLOW}wasp-app-runner is not installed.${RESET}"
    if ! install_latest_wasp_app_runner; then
      echo -e "${RED}Failed to install wasp-app-runner. Please check errors above.${RESET}"
      exit 1
    fi
    echo -e "${GREEN}wasp-app-runner is now installed.${RESET}"
    exit 0
  fi

  echo -e "Checking for wasp-app-runner updates..."

  local INSTALLED_VERSION
  INSTALLED_VERSION=$(get_installed_wasp_app_runner_version)

  local LATEST_VERSION
  LATEST_VERSION=$(get_latest_wasp_app_runner_version_from_github)

  if [[ "$INSTALLED_VERSION" == "$LATEST_VERSION" ]]; then
    echo -e "${GREEN}wasp-app-runner (version $INSTALLED_VERSION) is up to date.${RESET}"
  else
    echo -e "${YELLOW}A new version ($LATEST_VERSION) of wasp-app-runner is available (installed: $INSTALLED_VERSION).${RESET}"
    if ! install_latest_wasp_app_runner; then
      echo -e "${RED}Failed to update wasp-app-runner. Please check errors above.${RESET}"
      exit 1
    fi
    echo -e "${GREEN}wasp-app-runner has been updated to version ${LATEST_VERSION}.${RESET}"
  fi
  exit 0
}

is_wasp_app_runner_installed() {
  command -v run-wasp-app > /dev/null 2>&1
}

get_installed_wasp_app_runner_version() {
  version=$(run-wasp-app --version 2> /dev/null | tr -d '[:space:]')
  if [[ -z "$version" ]]; then
    echo "unknown"
  else
    echo "$version"
  fi
}

get_latest_wasp_app_runner_version_from_github() {
  local latest_version
  latest_version=$(curl -sL "https://raw.githubusercontent.com/wasp-lang/runner-action/main/package.json" | grep '"version":' | sed -n 's/.*"version": *"\([^"]*\)".*/\1/p' 2> /dev/null)
  if [[ -n "$latest_version" ]]; then
    echo "$latest_version"
  else
    echo "unknown"
  fi
}

install_latest_wasp_app_runner() {
  echo -e "Installing the latest version of wasp-app-runner..."

  local TMP_INSTALL_DIR
  TMP_INSTALL_DIR=$(mktemp -d "${PROJECT_ROOT}/tmp.XXXXXXXXXX")

  if [[ -z "$TMP_INSTALL_DIR" || ! -d "$TMP_INSTALL_DIR" ]]; then
    echo -e "${RED}Failed to create temporary directory for runner installation.${RESET}"
    return 1
  fi

  # Ensure cleanup of the temporary directory on exit, error, or interrupt
  trap 'rm -rf "$TMP_INSTALL_DIR" >/dev/null 2>&1' EXIT TERM INT

  local REPO_DIR="$TMP_INSTALL_DIR/runner-action"

  echo -e "Cloning wasp-app-runner to $REPO_DIR..."
  if ! git clone --depth 1 https://github.com/wasp-lang/runner-action.git "$REPO_DIR"; then
    echo -e "${RED}Failed to clone wasp-app-runner repository into $REPO_DIR!${RESET}"
    return 1
  fi

  cd "$REPO_DIR" || {
    echo -e "${RED}Failed to cd into $REPO_DIR${RESET}"
    return 1
  }

  echo -e "Building Wasp App Runner from $REPO_DIR..."
  if ! npm ci --silent; then
    echo -e "${RED}npm ci failed in $REPO_DIR${RESET}"
    return 1
  fi

  echo -e "Packing Wasp App Runner from $REPO_DIR..."
  local TAR_FILE
  TAR_FILE=$(npm pack --silent)
  if [[ -z "$TAR_FILE" || ! -f "$TAR_FILE" ]]; then
    echo -e "${RED}npm pack failed or produced no tarball in $REPO_DIR${RESET}"
    return 1
  fi
  echo -e "Packed to $TAR_FILE."

  echo -e "Installing Wasp App Runner globally from $REPO_DIR/$TAR_FILE..."
  if ! npm install -g "$REPO_DIR/$TAR_FILE"; then
    echo -e "${RED}npm install -g failed for $REPO_DIR/$TAR_FILE${RESET}"
    return 1
  fi

  trap - EXIT TERM INT # Clear the trap as we are done
  rm -rf "$TMP_INSTALL_DIR" > /dev/null 2>&1

  echo -e "${GREEN}wasp-app-runner installed/updated successfully.${RESET}"
  return 0
}

main_flow
