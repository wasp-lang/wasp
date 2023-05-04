#!/bin/sh -e

# Helper to navigate to the headless tests dir, install dependencies, and run the tests.

# Gets the directory of where this script lives.
dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

cd "$dir/../headless-test"
npm ci

# Installing PLaywright browsers
npx playwright install --with-deps

# Runs the tests with the debug flag so that we can see Wasp output
DEBUG=pw:webserver npx playwright test