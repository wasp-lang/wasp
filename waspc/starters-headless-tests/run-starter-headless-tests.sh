#!/bin/bash

# Script to run E2E tests for a given Wasp template.

set -e

if [ -z "$1" ]; then
  echo "Error: Wasp CLI command argument is missing."
  echo "Usage: $0 <wasp_cli_command> <template_name> <path_to_wasp_project> [path_to_headless_tests]"
  exit 1
fi

if [ -z "$2" ]; then
  echo "Error: Template name argument is missing."
  echo "Usage: $0 <wasp_cli_command> <template_name> <path_to_wasp_project> [path_to_headless_tests]"
  exit 1
fi

if [ -z "$3" ]; then
  echo "Error: Path to Wasp project argument is missing."
  echo "Usage: $0 <wasp_cli_command> <template_name> <path_to_wasp_project> [path_to_headless_tests]"
  exit 1
fi

WASP_CLI_CMD="$1"
TEMPLATE_NAME="$2"
WASP_PROJECT_PATH="$3"
HEADLESS_TESTS_PATH="${4}"
TEMP_PROJECT_NAME="temp-project-${TEMPLATE_NAME}"
TEMP_WASP_PROJECT_PATH="${TEMP_PROJECT_NAME}${WASP_PROJECT_PATH}"

main() {
  trap cleanup_test_environment EXIT INT TERM QUIT

  echo "Starting E2E tests for ${TEMPLATE_NAME} template..."
  initialize_test_environment
  run_dev_headless_tests
  run_build_headless_tests
  echo "Finished E2E tests for ${TEMPLATE_NAME} template"
}

initialize_test_environment() {
  cleanup_test_environment

  ${WASP_CLI_CMD} new "${TEMP_PROJECT_NAME}" -t "${TEMPLATE_NAME}"

  if [ -f "$TEMP_WASP_PROJECT_PATH/.env.server.example" ]; then
    cp "$TEMP_WASP_PROJECT_PATH/.env.server.example" "$TEMP_WASP_PROJECT_PATH/.env.server"
  fi
  
  if [ -f "$TEMP_WASP_PROJECT_PATH/.env.client.example" ]; then
    cp "$TEMP_WASP_PROJECT_PATH/.env.client.example" "$TEMP_WASP_PROJECT_PATH/.env.client"
  fi
}

cleanup_test_environment() {
  rm -rf "${TEMP_PROJECT_NAME}"
}

run_dev_headless_tests() {
  echo "Running DEV headless tests for ${TEMPLATE_NAME} project..."
  export DEBUG=pw:webserver
  export E2E_APP_PATH="./$TEMP_WASP_PROJECT_PATH"
  export WASP_CLI_CMD="${WASP_CLI_CMD}"
  export HEADLESS_TEST_MODE=dev
  npx playwright test --grep "(@${TEMPLATE_NAME}|^(?!.*@).*)"
}

run_build_headless_tests() {
  if template_uses_sqlite; then
    echo "Skipping BUILD tests for ${TEMPLATE_NAME} project"
    return
  fi

  echo "Running BUILD headless tests for ${TEMPLATE_NAME} project..."
  export DEBUG=pw:webserver
  export E2E_APP_PATH="./$TEMP_WASP_PROJECT_PATH"
  export WASP_CLI_CMD="${WASP_CLI_CMD}"
  export HEADLESS_TEST_MODE=build
  npx playwright test --grep "(@${TEMPLATE_NAME}|^(?!.*@).*)"
}

template_uses_sqlite() {
  cd "$TEMP_WASP_PROJECT_PATH"

  DATABASE_PROVIDER=$(${WASP_CLI_CMD} info \
    | grep "Database system" \
    | sed 's/.*: //' \
    | sed -e 's/\x1b\[[0-9;]*m//g' \
    | tr '[:upper:]' '[:lower:]')

  if [ -z "$DATABASE_PROVIDER" ]; then
    echo "ERROR: Could not determine database system from ${WASP_CLI_CMD} info"
    exit 1
  fi

  [ "$DATABASE_PROVIDER" = "sqlite" ]
}

main "$@"
