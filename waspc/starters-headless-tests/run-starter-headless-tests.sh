#!/bin/bash

# Script to run E2E tests for a given Wasp template.

set -e

if [ -z "$1" ]; then
  echo "Error: Template name argument is missing."
  echo "Usage: $0 <template_name> <wasp_cli_command>"
  exit 1
fi

if [ -z "$2" ]; then
  echo "Error: Wasp CLI command argument is missing."
  echo "Usage: $0 <template_name> <wasp_cli_command>"
  exit 1
fi

TEMPLATE_NAME="$1"
WASP_CLI_CMD="$2"
TEMP_PROJECT_NAME="temp-project-${TEMPLATE_NAME}"

main() {
  trap cleanup_test_environment EXIT

  echo "Starting E2E tests for ${TEMPLATE_NAME} template..."
  initialize_test_environment
  run_dev_headless_tests
  run_prod_headless_tests
  echo "Finished E2E tests for ${TEMPLATE_NAME} template"
}

initialize_test_environment() {
  cleanup_test_environment

  ${WASP_CLI_CMD} new "${TEMP_PROJECT_NAME}" -t "${TEMPLATE_NAME}"
}

cleanup_test_environment() {
  rm -rf "${TEMP_PROJECT_NAME}"
}

run_dev_headless_tests() {
  echo "Running DEV headless tests for ${TEMPLATE_NAME} project..."
  DEBUG=pw:webserver E2E_APP_PATH="./${TEMP_PROJECT_NAME}" WASP_CLI_CMD="${WASP_CLI_CMD}" HEADLESS_TEST_MODE=dev npx playwright test
}

run_prod_headless_tests() {
  if template_uses_sqlite; then
    echo "Skipping BUILD tests for ${TEMPLATE_NAME} project (sqlite detected in schema.prisma)."
    return
  fi

  echo "Running BUILD headless tests for ${TEMPLATE_NAME} project..."
  DEBUG=pw:webserver E2E_APP_PATH="./${TEMP_PROJECT_NAME}" WASP_CLI_CMD="${WASP_CLI_CMD}" HEADLESS_TEST_MODE=build npx playwright test
}

template_uses_sqlite() {
  local -r prisma_schema_file="./${TEMP_PROJECT_NAME}/schema.prisma"

  if [[ ! -f "$prisma_schema_file" ]]; then
    echo "Error: schema.prisma file not found at $prisma_schema_file" >&2
    echo "The project may not have been generated correctly." >&2
    return 1
  fi

  grep -q 'provider = "sqlite"' "$prisma_schema_file"
}

main "$@"
