#!/bin/bash

# Script to run E2E tests for a given Wasp template.

set -e

if [ -z "$1" ]; then
  echo "Error: Wasp CLI command argument is missing."
  echo "Usage: $0 <wasp_cli_command> <template_name> <path_to_wasp_project>"
  exit 1
fi

if [ -z "$2" ]; then
  echo "Error: Template name argument is missing."
  echo "Usage: $0 <wasp_cli_command> <template_name> <path_to_wasp_project>"
  exit 1
fi

if [ -z "$3" ]; then
  echo "Error: Path to Wasp project argument is missing."
  echo "Usage: $0 <wasp_cli_command> <template_name> <path_to_wasp_project>"
  exit 1
fi

WASP_CLI_CMD="$1"
TEMPLATE_NAME="$2"
WASP_PROJECT_RELATIVE_PATH="$3"
# TODO: Implement remote tests
# HEADLESS_TESTS_RELATIVE_PATH="${4}"
TEMP_PROJECT_NAME="temp-project-${TEMPLATE_NAME}"
TEMP_WASP_PROJECT_PATH="${TEMP_PROJECT_NAME}${WASP_PROJECT_RELATIVE_PATH}"

main() {
  trap cleanup_test_environment EXIT INT TERM QUIT

  initialize_test_environment
  run_dev_e2e_tests
  run_build_e2e_tests
}

initialize_test_environment() {
  cleanup_test_environment

  ${WASP_CLI_CMD} new "${TEMP_PROJECT_NAME}" -t "${TEMPLATE_NAME}"

  # Starters don't have .env.server.headless (like todoApp)
  # as we don't want to generate those files for end users.

  if [ -f "$TEMP_WASP_PROJECT_PATH/.env.server.example" ]; then
    cp "$TEMP_WASP_PROJECT_PATH/.env.server.example" "$TEMP_WASP_PROJECT_PATH/.env.server"
  else
    touch "$TEMP_WASP_PROJECT_PATH/.env.server"
  fi

  if [ -f "$TEMP_WASP_PROJECT_PATH/.env.client.example" ]; then
    cp "$TEMP_WASP_PROJECT_PATH/.env.client.example" "$TEMP_WASP_PROJECT_PATH/.env.client"
  fi

  # Add email verification skip for dev mode
  cat >> "$TEMP_WASP_PROJECT_PATH/.env.server" << EOF

SKIP_EMAIL_VERIFICATION_IN_DEV=true
EOF

  # Replace email provider with SMTP so it works with `wasp-app-runner`
  sed -i '' 's/provider: [A-Za-z0-9_][A-Za-z0-9_]*/provider: SMTP/g' "$TEMP_WASP_PROJECT_PATH/main.wasp"
  cat >> "$TEMP_WASP_PROJECT_PATH/.env.server" << EOF

SMTP_HOST=localhost
SMTP_USERNAME=any
SMTP_PASSWORD=any
SMTP_PORT=1025
EOF
}

cleanup_test_environment() {
  rm -rf "${TEMP_PROJECT_NAME}"
}

run_dev_e2e_tests() {
  echo "Running DEV e2e tests for ${TEMPLATE_NAME} project..."
  # export DEBUG=pw:webserver
  export E2E_APP_PATH="./$TEMP_WASP_PROJECT_PATH"
  export WASP_CLI_CMD="${WASP_CLI_CMD}"
  export HEADLESS_TEST_MODE=dev
  npx playwright test --grep "(@${TEMPLATE_NAME}|^(?!.*@).*)"
}

run_build_e2e_tests() {
  if template_uses_sqlite; then
    echo "Skipping BUILD tests for ${TEMPLATE_NAME} project"
    return
  fi

  echo "Running BUILD e2e tests for ${TEMPLATE_NAME} project..."
  # export DEBUG=pw:webserver
  export E2E_APP_PATH="./$TEMP_WASP_PROJECT_PATH"
  export WASP_CLI_CMD="${WASP_CLI_CMD}"
  export HEADLESS_TEST_MODE=build
  npx playwright test --grep "(@${TEMPLATE_NAME}|^(?!.*@).*)"
}

template_uses_sqlite() {
  (
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
  )
}

main "$@"
