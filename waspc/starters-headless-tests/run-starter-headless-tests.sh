#!/bin/bash

# Script to run E2E tests for a given Wasp template.

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
  # Ensure cleanup runs even if tests fail
  trap cleanup_test_environment EXIT

  # Group the main sequence of operations whose collective exit status needs to be captured
  local exit_code=0
  (
    initialize_test_environment \
      && run_dev_e2e_tests \
      && if template_uses_sqlite; then
        echo "Skipping BUILD tests for ${TEMPLATE_NAME} project (sqlite detected in schema.prisma)."
      else
        run_prod_e2e_tests
      fi
  )
  exit_code=$?

  echo "Finished E2E tests for ${TEMPLATE_NAME} template with exit code ${exit_code}"
  exit "${exit_code}"
}

cleanup_test_environment() {
  echo "Cleaning up ${TEMPLATE_NAME} project: ${TEMP_PROJECT_NAME}"
  rm -rf "${TEMP_PROJECT_NAME}"
}

initialize_test_environment() {
  echo "Starting E2E tests for ${TEMPLATE_NAME} template..."

  echo "Cleaning up any pre-existing temporary project directory: ${TEMP_PROJECT_NAME}"
  rm -rf "${TEMP_PROJECT_NAME}"

  echo "Generating ${TEMPLATE_NAME} project: ${TEMP_PROJECT_NAME}"
  ${WASP_CLI_CMD} new "${TEMP_PROJECT_NAME}" -t "${TEMPLATE_NAME}"
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

run_dev_e2e_tests() {
  echo "Running DEV tests for ${TEMPLATE_NAME} project..."
  DEBUG=pw:webserver E2E_APP_PATH="./${TEMP_PROJECT_NAME}" WASP_CLI_CMD=${WASP_CLI_CMD} HEADLESS_TEST_MODE=dev npx playwright test
}

run_prod_e2e_tests() {
  echo "Running BUILD tests for ${TEMPLATE_NAME} project..."
  E2E_APP_PATH="./${TEMP_PROJECT_NAME}" WASP_CLI_CMD=${WASP_CLI_CMD} HEADLESS_TEST_MODE=build npx playwright test
}

main "$@"
