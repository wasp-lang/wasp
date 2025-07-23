#!/bin/bash

# Extract database provider from Wasp CLI info output

# Usage: get-wasp-database-provider.sh <command>
# Where <command> is either 'wasp' or 'wasp-cli'

# Returns the provider name in lowercase

set -e

if [ $# -ne 1 ]; then
  echo "ERROR: Usage: $0 <command>" >&2
  echo "       Where <command> is either 'wasp' or 'wasp-cli'" >&2
  exit 1
fi

WASP_COMMAND="$1"

if [ "$WASP_COMMAND" != "wasp" ] && [ "$WASP_COMMAND" != "wasp-cli" ]; then
  echo "ERROR: Command must be either 'wasp' or 'wasp-cli', got: $WASP_COMMAND" >&2
  exit 1
fi

# Take the database line
# Take everything after the colon
# Remove ANSI color codes
# Convert to lowercase
DATABASE_PROVIDER=$($WASP_COMMAND info \
  | grep "Database system" \
  | sed 's/.*: //' \
  | sed -e 's/\x1b\[[0-9;]*m//g' \
  | tr '[:upper:]' '[:lower:]')

if [ -z "$DATABASE_PROVIDER" ]; then
  echo "ERROR: Could not determine database system from $WASP_COMMAND info" >&2
  exit 1
fi

echo "$DATABASE_PROVIDER"
