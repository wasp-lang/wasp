#!/usr/bin/env bash

# This script ensures wasp-app-runner is installed and up-to-date.

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
PROJECT_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)

WASP_APP_RUNNER_COMMAND="run-wasp-app"

main_flow() {
  if ! is_wasp_app_runner_installed; then
    echo_warning "wasp-app-runner is not installed."
    if ! install_latest_wasp_app_runner; then
      echo_error "Failed to install wasp-app-runner. Please check errors above."
      exit 1
    fi
    echo_success "wasp-app-runner is now installed."
    exit 0
  fi

  local installed_wasp_app_runner_version
  installed_wasp_app_runner_version=$(get_installed_wasp_app_runner_version)

  local latest_wasp_app_runner_version
  latest_wasp_app_runner_version=$(get_latest_wasp_app_runner_version_from_github)

  if ! [[ "$installed_wasp_app_runner_version" == "$latest_wasp_app_runner_version" ]]; then
    if ! install_latest_wasp_app_runner; then
      echo_error "Failed to update wasp-app-runner. Please check errors above."
      exit 1
    fi
    echo_success "wasp-app-runner has been updated to version ${latest_wasp_app_runner_version}."
  fi
  exit 0
}

is_wasp_app_runner_installed() {
  command -v "$WASP_APP_RUNNER_COMMAND" > /dev/null 2>&1
}

get_installed_wasp_app_runner_version() {
  local installed_version
  installed_version=$("$WASP_APP_RUNNER_COMMAND" --version 2> /dev/null | tr -d '[:space:]')
  if [[ -z "$installed_version" ]]; then
    echo "unknown"
  else
    echo "$installed_version"
  fi
}

get_latest_wasp_app_runner_version_from_github() {
  local latest_version
  latest_version=$(curl -sL "https://raw.githubusercontent.com/wasp-lang/runner-action/main/package.json" | grep '"version":' | sed -n -E 's/.*"version": *"([^"]*)".*/\1/p' 2> /dev/null)
  if [[ -z "$latest_version" ]]; then
    echo "unknown"
  else
    echo "$latest_version"
  fi
}

install_latest_wasp_app_runner() {
  local tmp_installation_dir
  if ! tmp_installation_dir=$(mktemp -d "${PROJECT_ROOT}/wasp-app-runner.XXXXXXXXXX"); then
    echo_error "Failed to create temporary directory for runner installation."
    return 1
  fi

  # Ensure cleanup of the temporary directory on exit, error, or interruption.
  # shellcheck disable=SC2064 # Shellcheck wants us to use single quotes here.
  # We intentionally use double quotes to capture the variable's current value,
  # instead of deferring the expansion until the trap runs (which single quotes would do).
  trap "rm -rf $tmp_installation_dir >/dev/null 2>&1" EXIT TERM INT

  if ! git clone --depth 1 --quiet https://github.com/wasp-lang/runner-action.git "$tmp_installation_dir"; then
    echo_error "Failed to clone wasp-app-runner repository into $tmp_installation_dir!"
    return 1
  fi

  cd "$tmp_installation_dir" || {
    echo_error "Failed to cd into $tmp_installation_dir"
    return 1
  }

  if ! npm ci --silent; then
    echo_error "npm ci failed in $tmp_installation_dir"
    return 1
  fi

  local tarball_filename
  if ! tarball_filename=$(npm pack --silent); then
    echo_error "npm pack failed in $tmp_installation_dir"
    return 1
  fi

  if ! npm install -g --silent "$tmp_installation_dir/$tarball_filename"; then
    echo_error "npm install -g failed for $tmp_installation_dir/$tarball_filename"
    return 1
  fi

  return 0
}

RESET="\033[0m"
YELLOW="\033[33m"
GREEN="\033[32m"
RED="\033[31m"

echo_error() {
  echo -e "${RED}$1${RESET}"
}

echo_success() {
  echo -e "${GREEN}$1${RESET}"
}

echo_warning() {
  echo -e "${YELLOW}$1${RESET}"
}

main_flow
