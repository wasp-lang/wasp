#!/bin/bash

# Extract database provider from Wasp CLI info output

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

WASP_INFO_OUTPUT=$($WASP_COMMAND info 2>&1) || {
  echo "ERROR: '$WASP_COMMAND info' failed with exit code $?:" >&2
  echo $WASP_INFO_OUTPUT >&2
  exit 1
}

# Take the database line
# Take everything after the colon
# Remove ANSI color codes
# Convert to lowercase
DATABASE_PROVIDER=$(echo "$WASP_INFO_OUTPUT" \
  | grep "Database system" \
  | sed 's/.*: //' \
  | sed -e 's/\x1b\[[0-9;]*m//g' \
  | tr '[:upper:]' '[:lower:]')

if [ -z "$DATABASE_PROVIDER" ]; then
  echo "ERROR: Could not determine database system from $WASP_COMMAND info" >&2
  exit 1
fi

echo "$DATABASE_PROVIDER"
