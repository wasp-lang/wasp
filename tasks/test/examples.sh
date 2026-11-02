#!/usr/bin/env bash
#MISE description="Executes only examples e2e tests. Installs deps first if needed."
#MISE depends=["build-wasp-app-runner"]
set -e

REPOSITORY_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
WASP_CLI_CMD="$REPOSITORY_ROOT/waspc/tools/wasp-cli-dev"
export WASP_CLI_CMD
export WASP_APP_RUNNER_CLI_CMD="$REPOSITORY_ROOT/packages/wasp-app-runner/bin/index.js"

paths=(
  "examples/tutorials/TodoApp"
  "examples/tutorials/TodoAppTs"
  "examples/waspello"
  "examples/waspleau"
  "examples/websockets-realtime-voting"
  "examples/ask-the-documents"
  "examples/kitchen-sink"
)

# Copies .env.server.example to .env.server in the current directory,
# but only if the example exists and .env.server isn't already present.
copy_env_server_example_if_needed() {
  [ ! -f .env.server.example ] || [ -f .env.server ] || cp .env.server.example .env.server
}

test_example() {
  cd "$REPOSITORY_ROOT/$1" && copy_env_server_example_if_needed && "$WASP_CLI_CMD" install && npm run test
}

for path in "${paths[@]}"; do
  if ! (test_example "$path"); then
    echo -e "\033[31mE2E tests failed in $path\033[0m"
    exit 1
  fi
done
