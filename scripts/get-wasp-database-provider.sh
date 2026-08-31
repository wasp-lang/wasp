#!/bin/bash

# Extract database provider from the Wasp CLI's app spec

# Usage: get-wasp-database-provider.sh <command>
# Where <command> is whatever invokes Wasp's CLI in the current environment,
# e.g. 'wasp', 'wasp-cli', 'cabal -v0 --project-dir=/path/to/waspc run wasp-cli --'...

# Returns the provider name in lowercase

set -e

if [ $# -ne 1 ]; then
  echo "ERROR: Usage: $0 <command>" >&2
  exit 1
fi

WASP_COMMAND="$1"

# NOTE: We don't redirect stderr into stdout, as the CLI prints compilation
# diagnostics to stderr and we need stdout to stay valid JSON.
WASP_SPEC_OUTPUT=$($WASP_COMMAND show spec --json) || {
  echo "ERROR: '$WASP_COMMAND show spec --json' failed with exit code $?" >&2
  exit 1
}

DATABASE_PROVIDER=$(echo "$WASP_SPEC_OUTPUT" \
  | jq -r '.dbSystem' \
  | tr '[:upper:]' '[:lower:]')

if [ -z "$DATABASE_PROVIDER" ] || [ "$DATABASE_PROVIDER" = "null" ]; then
  echo "ERROR: Could not determine database system from $WASP_COMMAND show spec --json" >&2
  exit 1
fi

echo "$DATABASE_PROVIDER"
