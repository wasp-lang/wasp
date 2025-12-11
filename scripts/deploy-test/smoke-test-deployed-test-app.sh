#!/usr/bin/env bash

# This script performs smoke tests on deployed Wasp applications to verify
# that both the server and client app are running and accessible.
# Called from `.github/workflows/ci-deploy-test.yaml`.

set -euo pipefail

readonly MAX_RETRIES=10
readonly RETRY_DELAY_SECONDS=30
readonly CURL_RETRY_OPTS="--fail --retry $MAX_RETRIES --retry-all-errors --retry-delay $RETRY_DELAY_SECONDS --show-error"
readonly EXPECTED_CLIENT_TEXT="Wasp Kitchen Sink"

log() {
  local timestamp
  timestamp=$(date +%H:%M:%S)
  echo "[$timestamp] $*"
}

parse_args() {
  if [[ $# -ne 2 ]]; then
    echo "Usage: $0 <server-hostname> <client-hostname>" >&2
    exit 1
  fi

  SERVER_URL="https://$1"
  CLIENT_URL="https://$2"
}

smoke_test_server() {
  local url="${SERVER_URL}/operations/get-date"
  log "[Server] Hitting $url"

  local response
  response=$(curl $CURL_RETRY_OPTS -X POST -H "Content-Type: application/json" "$url")

  if ! echo "$response" | jq -e '.json' > /dev/null; then
    echo "Server response missing expected \"json\" field" >&2
    return 1
  fi

  log "[Server] Success"
}

smoke_test_client() {
  log "[Client] Hitting $CLIENT_URL"

  local response
  response=$(curl $CURL_RETRY_OPTS "$CLIENT_URL")

  if ! echo "$response" | grep -q "$EXPECTED_CLIENT_TEXT"; then
    echo "Client HTML does not contain expected text '$EXPECTED_CLIENT_TEXT'" >&2
    return 1
  fi

  log "[Client] Success"
}

main() {
  parse_args "$@"

  smoke_test_server
  smoke_test_client

  log "All smoke tests passed"
}

main "$@"
