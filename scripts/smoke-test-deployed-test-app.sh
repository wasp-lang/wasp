#!/usr/bin/env bash
set -euo pipefail

MAX_RETRIES=5
INITIAL_WAIT_SECONDS=2
TIMEOUT_SECONDS=30

usage() {
    echo "Usage: $0 <server_hostname> <client_hostname>"
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
    usage
    exit 0
fi

if [[ $# -ne 2 ]]; then
    echo "Error: Missing required arguments"
    usage
    exit 1
fi

SERVER_HOSTNAME="$1"
CLIENT_HOSTNAME="$2"
SERVER_URL="https://$SERVER_HOSTNAME"
CLIENT_URL="https://$CLIENT_HOSTNAME"

retry_with_backoff() {
    local name="$1"; shift
    local attempt=0
    local wait_time=0
    while [[ $attempt -lt $MAX_RETRIES ]]; do
        echo "[$name] Attempt $((attempt + 1))/$MAX_RETRIES at $(date -u +'%H:%M:%S')"
        if "$@"; then
            echo "[$name] Success"
            return 0
        fi
        attempt=$((attempt + 1))
        if [[ $attempt -lt $MAX_RETRIES ]]; then
            wait_time=$(( INITIAL_WAIT_SECONDS * (2 ** (attempt - 1)) ))
            echo "[$name] Waiting ${wait_time}s before retry..."
            sleep "$wait_time"
        else
            echo "[$name] Failed after $MAX_RETRIES attempts"
            return 1
        fi
    done
}

server_check_once() {
    echo "[Server] Hitting $SERVER_URL/operations/get-date"
    curl --fail --silent --max-time "$TIMEOUT_SECONDS" -X POST \
        "$SERVER_URL/operations/get-date" \
      | jq -e '.json' > /dev/null
}

client_check_once() {
    echo "[Client] Hitting $CLIENT_URL"
    curl --fail --silent --max-time "$TIMEOUT_SECONDS" \
        "$CLIENT_URL" \
      | grep -q 'ToDo App'
}

echo "Server URL: $SERVER_URL"
echo "Client URL: $CLIENT_URL"

if ! retry_with_backoff "Server" server_check_once; then
    echo "Server smoke test failed"
    exit 1
fi

if ! retry_with_backoff "Client" client_check_once; then
    echo "Client smoke test failed"
    exit 1
fi

echo "All smoke tests passed"