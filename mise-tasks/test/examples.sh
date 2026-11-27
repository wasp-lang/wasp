#!/usr/bin/env bash
#MISE description="Executes only examples e2e tests. Installs deps, compiles, etc first. Tests all examples by default, or only the specified ones (e.g. `kitchen-sink`)."
#MISE depends=["build-wasp-app-runner"]
set -e

REPOSITORY_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
EXAMPLES_DIR="$REPOSITORY_ROOT/examples"
export WASP_CLI_CMD="${WASP_CLI_CMD:-$REPOSITORY_ROOT/waspc/tools/wasp-cli-dev}"
export WASP_APP_RUNNER_CLI_CMD="$REPOSITORY_ROOT/packages/wasp-app-runner/bin/index.js"

# Paths are relative to the examples dir.
all_examples=(
  "tutorials/TodoApp"
  "tutorials/TodoAppTs"
  "waspello"
  "waspleau"
  "websockets-realtime-voting"
  "ask-the-documents"
  "kitchen-sink"
)

examples_to_test=("${@:-${all_examples[@]}}")

# Copies .env.server.example to .env.server in the current directory,
# but only if the example exists and .env.server isn't already present.
copy_env_server_example_if_needed() {
  [ ! -f .env.server.example ] || [ -f .env.server ] || cp .env.server.example .env.server
}

test_example() {
  cd "$EXAMPLES_DIR/$1" || return 1
  copy_env_server_example_if_needed || return 1
  "$WASP_CLI_CMD" install || return 1

  if [ "${WASP_RUN_MODE:-}" = "build" ] && uses_sqlite; then
    echo -e "\033[33mSkipping examples/$1: it uses SQLite, which Wasp doesn't support in build mode\033[0m"
    return 0
  fi

  "$WASP_CLI_CMD" compile && npm run test
}

# Tells if the Wasp app in the current directory uses SQLite. Needs its deps to be installed.
uses_sqlite() {
  [ "$("$REPOSITORY_ROOT/scripts/get-wasp-database-provider.sh" "$WASP_CLI_CMD")" = "sqlite" ]
}

for example in "${examples_to_test[@]}"; do
  if ! (test_example "$example"); then
    echo -e "\033[31mE2E tests failed in examples/$example\033[0m"
    exit 1
  fi
done
